{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Lib where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import qualified Euterpea as MIDI

import SolReSol

data SingData = SingData { 
  device :: Int,
  tempo :: Rational,
  ready :: TVar Bool
}

type Singer a = StateT SingData IO a

class (Singable a) where 
  toSong  :: a -> MIDI.Music1

instance {-# OVERLAPPABLE #-} (SolReSol a) => Singable a where
  toSong = MIDI.toMusic1 . toMusic

instance Singable (MIDI.Music MIDI.Pitch) where
  toSong = MIDI.toMusic1

instance Singable (MIDI.Music (MIDI.Pitch, [MIDI.NoteAttribute])) where
  toSong = id

forkSchedulingIO :: TVar Bool -> IO () -> TVar Bool -> IO ThreadId
forkSchedulingIO trigger action isDone = forkIO $ do
  atomically $ do
    isReady <- readTVar trigger
    check isReady
  action 
  atomically $ writeTVar isDone True

schedule :: IO () -> Singer ()
schedule action = do
  state <- get
  forkReady <- lift $ newTVarIO False
  lift $ forkSchedulingIO (ready state) action forkReady
  put (state {ready = forkReady})

doIO :: (a -> IO ()) -> a -> Singer ()
doIO f arg = schedule $ f arg

doIONow :: (a -> IO b) -> a -> Singer b
doIONow f arg = lift $ f arg

sing :: Singable a => a -> Singer ()
sing s = do
  let melody = toSong s
  state <- get
  schedule $ MIDI.playDev (device state) melody

whileSinging :: Singable a => a -> IO () -> Singer ()
whileSinging s io = do
  let melody = toSong s
  state <- get
  schedule $ do
    thread <- forkIO io
    MIDI.playDev (device state) melody
    killThread thread

sync :: Singer ()
sync = do
  state <- get
  lift $ atomically $ readTVar (ready state) >>= check

execSinger :: Singer a -> Int -> IO a
execSinger s d = do
  firstReady <- newTVarIO True
  evalStateT s (SingData d 1.0 firstReady)
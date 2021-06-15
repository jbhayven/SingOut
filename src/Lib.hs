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

playWhenAvailable :: Singable a => SingData -> TVar Bool -> a -> IO ()
playWhenAvailable (SingData d t canTake) canYield singable = do
  let melody = toSong singable
  atomically $ do
    ready <- readTVar canTake
    check ready
  MIDI.playDev d melody
  atomically $ writeTVar canYield True

singMe :: Singable a => a -> Singer ()
singMe s = do
  singData <- get
  forkReady <- lift $ newTVarIO False
  lift $ forkIO $ playWhenAvailable singData forkReady s
  put (singData {ready = forkReady})

sync :: Singer ()
sync = do
  state <- get
  lift $ atomically $ readTVar (ready state) >>= check

execSinger :: Singer a -> Int -> IO a
execSinger s d = do
  firstReady <- newTVarIO True
  evalStateT s (SingData d 1.0 firstReady)
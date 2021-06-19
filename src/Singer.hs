{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Singer where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad.State
import qualified Euterpea as MIDI

import SolReSol

data SingData = SingData { 
  device :: Int,
  tempo :: Rational,
  channel :: Chan (IO ()),
  counter :: TMVar Integer
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

modifyCounter :: TMVar Integer -> (Integer -> Integer) -> IO ()
modifyCounter counter f = atomically $ do
  curr <- takeTMVar counter
  putTMVar counter (f curr)

handleOutputs :: Chan (IO ()) -> TMVar Integer -> IO ()
handleOutputs channel counter = forever $ do 
  ioAction <- readChan channel
  ioAction
  modifyCounter counter (+(-1))

schedule :: IO () -> Singer ()
schedule action = do
  state <- get
  lift $ modifyCounter (counter state) (+1)
  lift $ writeChan (channel state) action

doIO :: (a -> IO ()) -> a -> Singer ()
doIO f arg = schedule $ f arg

doIONow :: (a -> IO b) -> a -> Singer b
doIONow f arg = lift $ f arg

sing :: Singable a => a -> Singer ()
sing s = do
  let melody = toSong s
  state <- get
  schedule $ play state melody
  
play :: SingData -> MIDI.Music1 -> IO ()
play state melody = do
  MIDI.playDev (device state) melody
  threadDelay 2000000 -- perhaps a hack; needed to avoid glitches in my setup

whileSinging :: Singable a => a -> IO () -> Singer ()
whileSinging s io = do
  let melody = toSong s
  state <- get
  schedule $ do
    thread <- forkIO io
    play state melody
    killThread thread

sync :: Singer ()
sync = do
  state <- get
  lift $ atomically $ do
    curr <- readTMVar (counter state) 
    check (curr == 0)

loopSinger :: Singer a -> Singer ()
loopSinger s = s >> (loopSinger s) -- lift $ forever s 

forkSinger :: Singer () -> Singer ThreadId
forkSinger s = do 
  currState <- get 
  lift $ forkIO $ execSingerWithData s currState

execSingerWithData :: Singer a -> SingData -> IO a 
execSingerWithData s singData = evalStateT s singData

execSinger :: Singer a -> Int -> IO a
execSinger s d = do
  channel <- newChan
  counter <- newTMVarIO 0
  forkIO $ handleOutputs channel counter
  execSingerWithData s (SingData d 1.0 channel counter)
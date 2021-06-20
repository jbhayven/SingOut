{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Singer (
  Singer,
  InstrumentName (..),
  Instrument,
  devices,
  liftIO,
  doIO, 
  doIONow, 
  sing, 
  whileSinging, 
  sync, 
  loopSinger, 
  forkSinger, 
  setVoice, 
  transposeVoice,
  setRelativeTempo, 
  resetTempo, 
  resetTransposition, 
  resetModifiers,
  execSinger
) where 

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad.State
import Euterpea (
  InstrumentName (..), Music, Music1, Pitch, NoteAttribute, 
  devices, toMusic1, playDev, changeInstrument, scaleDurations, shiftPitches1)

import SolReSol

type Instrument = InstrumentName

data SingData = SingData { 
  device :: Int,
  defaultInstr :: Instrument,
  tempo :: Rational,
  trans :: Int,
  channel :: Chan (IO ()),
  counter :: TMVar Integer
}

type Singer a = StateT SingData IO a

class (Singable a) where 
  toSong  :: InstrumentName -> a -> Music1

instance {-# OVERLAPPABLE #-} (SolReSol a) => Singable a where
  toSong instr = toMusic1 . (changeInstrument instr) . toMusic

instance Singable (Music Pitch) where
  toSong _ = toMusic1

instance Singable (Music (Pitch, [NoteAttribute])) where
  toSong _ = id

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
  
play :: SingData -> Music1 -> IO ()
play state melody = do
  let scaled     = scaleDurations (tempo state) melody
      transposed = shiftPitches1 (trans state) scaled
  playDev (device state) transposed

doIO :: (a -> IO ()) -> a -> Singer ()
doIO f arg = schedule $ f arg

doIONow :: (a -> IO b) -> a -> Singer b
doIONow f arg = lift $ f arg

-- Schedules an object to be sung out.
sing :: Singable a => a -> Singer ()
sing s = do
  state <- get
  let melody = toSong (defaultInstr state) s
  schedule $ play state melody

-- Allows to schedule an IO action performed
-- over the duration of the singing.
whileSinging :: Singable a => a -> IO () -> Singer ()
whileSinging s io = do
  state <- get
  let melody = toSong (defaultInstr state) s
  schedule $ do
    thread <- forkIO io
    play state melody
    killThread thread

-- Flushes the singer output 
-- (waits for all songs and other deferred IOs to complete)
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

setVoice :: Instrument -> Singer ()
setVoice instr = do
  state <- get
  put (state { defaultInstr = instr })

setRelativeTempo :: Rational -> Singer ()
setRelativeTempo x = do
  state <- get
  put (state { tempo = tempo state * x })

resetTempo :: Singer ()
resetTempo = do
  state <- get
  put (state { tempo = 1.0 })

transposeVoice :: Int -> Singer ()
transposeVoice pitch = do
  state <- get
  put (state { trans = (trans state) + pitch })

resetTransposition :: Singer ()
resetTransposition = do
  state <- get
  put (state { trans = 0 })

resetModifiers :: Singer ()
resetModifiers = do 
  SingData dev _ _ _ chan cnt <- get
  put $ SingData dev ChoirAahs 1.0 0 chan cnt

execSingerWithData :: Singer a -> SingData -> IO a 
execSingerWithData s singData = evalStateT s singData

execSinger :: Singer a -> Int -> IO a
execSinger s d = do
  channel <- newChan
  counter <- newTMVarIO 0
  forkIO $ handleOutputs channel counter
  execSingerWithData s (SingData d ChoirAahs 1.0 0 channel counter)
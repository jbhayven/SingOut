{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Singer where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Monad.State
import qualified Euterpea as MIDI

import SolReSol

type Instrument =  MIDI.InstrumentName

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
  toSong  :: MIDI.InstrumentName -> a -> MIDI.Music1

instance {-# OVERLAPPABLE #-} (SolReSol a) => Singable a where
  toSong instr = MIDI.toMusic1 . (MIDI.changeInstrument instr) . toMusic

instance Singable (MIDI.Music MIDI.Pitch) where
  toSong _ = MIDI.toMusic1

instance Singable (MIDI.Music (MIDI.Pitch, [MIDI.NoteAttribute])) where
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

doIO :: (a -> IO ()) -> a -> Singer ()
doIO f arg = schedule $ f arg

doIONow :: (a -> IO b) -> a -> Singer b
doIONow f arg = lift $ f arg
  
play :: SingData -> MIDI.Music1 -> IO ()
play state melody = do
  let scaled     = MIDI.scaleDurations (tempo state) melody
      transposed = MIDI.shiftPitches1 (trans state) scaled
  MIDI.playDev (device state) transposed
  threadDelay 2000000 -- perhaps a hack; needed to avoid glitches in my setup

sing :: Singable a => a -> Singer ()
sing s = do
  state <- get
  let melody = toSong (defaultInstr state) s
  schedule $ play state melody

whileSinging :: Singable a => a -> IO () -> Singer ()
whileSinging s io = do
  state <- get
  let melody = toSong (defaultInstr state) s
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

transpose :: Int -> Singer ()
transpose pitch = do
  state <- get
  put (state { trans = (trans state) + pitch })

resetTransposition :: Singer ()
resetTransposition = do
  state <- get
  put (state { trans = 0 })

resetModifiers :: Singer ()
resetModifiers = do 
  SingData dev _ _ _ chan cnt <- get
  put $ SingData dev MIDI.ChoirAahs 1.0 0 chan cnt

liftIO :: IO a -> Singer a 
liftIO = lift

execSingerWithData :: Singer a -> SingData -> IO a 
execSingerWithData s singData = evalStateT s singData

execSinger :: Singer a -> Int -> IO a
execSinger s d = do
  channel <- newChan
  counter <- newTMVarIO 0
  forkIO $ handleOutputs channel counter
  execSingerWithData s (SingData d MIDI.ChoirAahs 1.0 0 channel counter)
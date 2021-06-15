{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Lib where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.State
import qualified Euterpea as MIDI

import SolReSol

data SingData = SingData { 
  device :: Int,
  instr :: MIDI.InstrumentName,
  tempo :: Rational,
  ready :: TVar Bool
}

type Singer a = StateT SingData IO a

class (Singable a) where 
  toSong  :: a -> MIDI.Music MIDI.Pitch

instance SolReSol a => Singable a where
  toSong = toMusic

instance Singable (MIDI.Music MIDI.Pitch) where
  toSong = id

updWith :: MIDI.Music a -> MIDI.InstrumentName -> Rational -> MIDI.Music a
updWith melody i t = MIDI.changeInstrument i $ MIDI.tempo t $ melody 

playWhenAvailable :: Singable a => SingData -> TVar Bool -> a -> IO ()
playWhenAvailable (SingData d i t canTake) canYield singable = do
  let melody = updWith (toSong singable) i t
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

execSinger :: Singer a -> Int -> IO a
execSinger s d = execSingerOn s d MIDI.Pad4Choir

execSingerOn ::  Singer a -> Int -> MIDI.InstrumentName -> IO a
execSingerOn s d i = do
  firstReady <- newTVarIO True
  evalStateT s (SingData d i 1.0 firstReady)

testSinger :: Singer ()
testSinger = do
  singMe (696969 :: Int) 
  lift $ threadDelay 10000000
  pure ()
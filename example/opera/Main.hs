{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Main where

import Singer

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BTS
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import Euterpea
import Network.HTTP.Client
import System.Environment
import System.IO

type Cue = (Int, String -> IO (), String)
type CueList = [Cue]

charDelay :: Int
charDelay = 100000

putChars :: String -> IO ()
putChars str = forM_ str (\x -> putChar x >> hFlush stdout >> threadDelay charDelay) 

ariaDiMezzoCarattere :: CueList
ariaDiMezzoCarattere = [
  (0, putStrLn, "Now playing: Maria & Draco"),
  (1000, putStrLn, ""),
  (3000, putChars, "The forces of the West fell, \nand Maria's castle was taken."),
  (12800, putChars, "\n\nPrince Ralse, of the East, took her hand by force.\nBut she never stopped yearning for Draco..."),
  (22500, putStrLn, ""),
  (23000, putStrLn, ""),
  (23500, putStrLn, "   MARIA:"),
  (24100, putChars, "♫ Oh"),
  (25000, putChars, " my"),
  (25700, putChars, " hero,"),
  (28000, putChars, "\n  So far away now."),
  (31300, putChars, "\n  Will I ever"),
  (33800, putChars, " see your smile?"),
  (38000, putChars, "\n\n♫ Love goes away,"),
  (41200, putChars, "\n  like night into day."),
  (44500, putChars, "\n♫ It's just a fading dream... ♫"),
  (51700, putChars, "\n\n♫ I'm the darkness,"),
  (55300, putChars, "\n  you're the stars."),
  (58600, putChars, "\n  Our love"),
  (60800, putChars, " is brighter than the sun."),
  (65400, putChars, "\n\n♫ For eternity,"),
  (68500, putChars, "\n  for me there can be"),
  (72000, putChars, "\n♫ only you, my chosen one... ♫"),
  (77900, putChars, "\n\n♫ Must I forget you?"),
  (81000, putChars, "\n  Our solemn promise?"),
  (84000, putChars, "\n  Will autumn take"),
  (86000, putChars, " the place of spring?"),
  (90500, putChars, "\n\n♫ What shall I do?"),
  (94000, putChars, "\n  I'm lost without you."),
  (97300, putChars, "\n  Speak"),
  (99000, putChars, " to me"),
  (100100, putChars, " once more! ♫"),
  (104000, putStrLn, ""),
  (104500, putStrLn, ""),
  (105000, putStrLn, "(A vision of DRACO appears)"),
  (106000, putChars, "\n   DRACO:"),
  (107000, putChars, "\n  Come, Maria!\n  Follow my lead..."),
  (113000, putStrLn, ""),
  (113500, putStrLn, ""),
  (114000, putStrLn, "(DRACO's apparition and MARIA dance)"),
  (126000, putChars, "\n   DRACO:\n  Ha, ha, ha..."),
  (129000, putStrLn, ""),
  (129500, putStrLn, ""),
  (130000, putStrLn, "(DRACO's ghost disappears, leaving a bouquet behind)"),
  (132500, putStrLn, ""),
  (133000, putStrLn, "(MARIA picks the bouquet)"),
  (135500, putStrLn, ""),
  (136000, putStrLn, "(MARIA climbs up the tower staircase)"),
  (150500, putStrLn, ""),
  (151000, putStrLn, "   MARIA:"),
  (151500, putChars, "♫ We must part now."),
  (154500, putChars, "\n  My life goes on."),
  (157500, putChars, "\n  But my heart won't give you up"),
  (160000, putStrLn, ""),
  (160500, putStrLn, ""),
  (161000, putStrLn, "(MARIA throws the bouquet up into the air and outside the tower parapet)"),
  (164000, putStrLn, ""),
  (164500, putStrLn, "   MARIA:"),
  (165000, putChars, "♫ Ere I walk away,"),
  (167500, putChars, "\n  let me hear you say."),
  (171000, putChars, "\n  I meant as much to you... ♫"),
  (177000, putChars, "\n\n♫ So gently,"),
  (180000, putChars, "\n  you touched my heart."),
  (183500, putChars, "\n  I will be"),
  (185500, putChars, " forever"),
  (187300, putChars, " yours."),
  (190500, putChars, "\n\n♫ Come what may,"),
  (193400, putChars, "\n  I won't age a day,"),
  (196700, putChars, "\n  I'll wait"),
  (198500, putChars, " for you"),
  (200000, putChars, " always... ♫"),
  (204000, putStrLn, ""),
  (204500, putStrLn, ""),
  (205000, putStrLn, "(A shooting star falls from the sky. MARIA watches it pass.)"),
  (208500, putStrLn, ""),
  (209000, putStrLn, "(The CHANCELLOR climbs towards MARIA)"),
  (210500, putStrLn, ""),
  (211000, putStrLn, "   CHANCELLOR:"),
  (211500, putStrLn, "Prince Ralse is looking for a dance partner."),
  (214000, putChars, "\nLeave the past behind!\nOur kingdom is adopting the spirit of the East...!"),
  (220000, putStrLn, ""),
  (220500, putStrLn, ""),
  (221000, putStrLn, "(The CHANCELLOR leads MARIA inside)"),
  (222000, putStrLn, ""),
  (222500, putStrLn, "(MARIA follows him)"),
  (223500, putStrLn, ""),
  (224000, putStrLn, "(She takes one last look at the night sky)"),
  (227500, putStrLn, ""),
  (228000, putStrLn, "(She turns her back and follows the CHANCELLOR)")]

performCues :: CueList -> IO ()
performCues cues = 
  let performCuesAux _ [] = pure ()
      performCuesAux startTime ((nextMilliseconds, action, str):t) = do
        -- putStrLn $ " " ++ show last ++ " " ++ show next
        now <- systemToTAITime <$> getSystemTime 
        let diff = (diffAbsoluteTime now startTime)
        let milliseconds = fromIntegral $ diffTimeToPicoseconds diff `div` 10 ^ 9

        threadDelay (1000 * (nextMilliseconds - milliseconds))  
        action str 
        performCuesAux startTime t
    
  in case cues of
    ((start, _, _):_) -> do
      startTime <- systemToTAITime <$> getSystemTime
      performCuesAux startTime cues
    _ -> undefined

prepareMIDI :: String -> String -> IO ()
prepareMIDI addr fileName = do
  putStrLn ("Downloading the music (MIDI) file from " ++ addr ++ "...")
  manager <- newManager defaultManagerSettings
  request <- parseRequest ("GET " ++ addr) 
  response <- httpLbs request manager

  BTS.writeFile fileName (C.toStrict $ responseBody response)
  putStrLn "File saved!"

introduction :: Singer ()
introduction = do
  -- Maybe something that makes more sense?
  setVoice Timpani
  transposeVoice (-24)
  setRelativeTempo 0.5
  sing "do dodo  do"
  resetModifiers 

performOpera :: String -> CueList -> Singer ()
performOpera fileName cues = do
  doIONow putStrLn "Scheduling the performance..."
  celes <- liftIO $ importFile fileName
  case celes of
    Left err -> doIONow print err
    Right mid -> whileSinging (fromMidi mid) (performCues cues)
  doIONow putStrLn "Scheduling complete. Enjoy!\n"
  sync

opera :: Singer ()
opera = do
  introduction

  liftIO $ prepareMIDI "http://www.fflyrics.com/midis/ff3celes.mid" "ff3celes.mid"

  performOpera "ff3celes.mid" ariaDiMezzoCarattere

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> devices
    (number : []) ->
      let device = read number in 
      execSinger opera device
      

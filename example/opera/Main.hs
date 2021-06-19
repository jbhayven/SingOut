{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Main where

import Singer

import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import Euterpea
import System.Environment
import System.IO

charDelay :: Int
charDelay = 100000

putChars :: String -> IO ()
putChars str = forM_ str (\x -> putChar x >> hFlush stdout >> threadDelay charDelay) 

cues :: [(Int, String -> IO (), String)]
cues = [
  (0, putStrLn, "Now playing: Maria & Draco"),
  (1, putStrLn, ""),
  (3, putChars, "The forces of the West fell, \nand Maria's castle was taken."),
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
  (38000, putChars, "\n♫ Love goes away,"),
  (41200, putChars, "\n  like night into day."),
  (44500, putChars, "\n♫ It's just a fading dream... ♫"),
  (51700, putChars, "\n\n♫ I'm the darkness,"),
  (55300, putChars, "\n  you're the stars."),
  (58600, putChars, "\n  Our love"),
  (60800, putChars, " is brighter than the sun."),
  (65400, putChars, "\n♫ For eternity,"),
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
  (105000, putStrLn, ""),
  (105000, putStrLn, "\n(A vision of DRACO appears)"),
  (106000, putStrLn, "\n   DRACO:"),
  (107000, putChars, "  Come, Maria!\n  Follow my lead..."),
  (114000, putStrLn, "\n\n(DRACO's apparition dances around MARIA)"),
  (122000, putStrLn, "\n(MARIA tries to follow him, but in vain)"),
  (126000, putChars, "\n   DRACO:\n  Ha, ha, ha..."),
  (130000, putStrLn, "\n\n(DRACO's ghost disappears, leaving a bouquet behind)"),
  (133000, putStrLn, "\n(MARIA picks the bouquet)"),
  (136000, putStrLn, "\n(MARIA climbs towards the rampart)"),
  (150500, putStrLn, ""),
  (151000, putStrLn, "   MARIA:"),
  (151500, putChars, "♫ We must part now."),
  (154500, putChars, "\n  My life goes on."),
  (157500, putChars, "\n  But my heart won't give you up"),
  (161000, putStrLn, "\n\n(MARIA throws the bouquet up into the air and outside the tower parapet)"),
  (164500, putStrLn, "\n   MARIA:"),
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
  (205000, putStrLn, "\n\n(A shooting star falls from the sky. MARIA watches it pass.)"),
  (209000, putStrLn, "\n(The CHANCELLOR climbs towards MARIA)"),
  (211000, putStrLn, "\n   CHANCELLOR:"),
  (211500, putStrLn, "Prince Ralse is looking for a dance partner."),
  (214000, putChars, "\nLeave the past behind!\nOur kingdom is adopting the spirit of the East...!"),
  (221000, putStrLn, "\n\n(The CHANCELLOR leads MARIA inside)"),
  (222000, putStrLn, "\n(MARIA follows him)"),
  (223000, putStrLn, "\n(She takes one last look at the night sky)"),
  (228000, putStrLn, "\n(MARIA turns her back and follows the CHANCELLOR)")]

performCues :: [(Int, String -> IO (), String)] -> IO ()
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

opera :: Singer ()
opera = do
  celes <- doIONow importFile "example/ff3celes.mid"
  case celes of
    Left err -> doIONow print err
    Right mid -> whileSinging (fromMidi mid) (performCues cues)
  doIONow putStrLn "Scheduling complete. Enjoy!"
  sync

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> devices
    (number : []) -> do
      let device = read number in execSinger opera device
      

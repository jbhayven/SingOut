{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Main where

import Lib

import Control.Concurrent
import Control.Monad.Trans
import Euterpea
import System.Environment
import System.IO

import HSoM.Examples.MUIExamples2

opera :: Singer ()
opera = do
  celes <- doIONow importFile "example/ff3celes.mid"
  case celes of
    Left err -> doIONow print err
    Right mid -> whileSinging (fromMidi mid) $ do
         putStrLn "Now playing: Maria & Draco"
         putStrLn ""
         threadDelay 5000000
         putStrLn "The forces of the West fell,"
         threadDelay 1000000
         putStrLn "and Maria's castle was taken."
         putStrLn ""
         threadDelay 5000000
         putStrLn "Prince Ralse, of the East, took her hand by force."
         threadDelay 1000000
         putStrLn "But she never stopped yearning for Draco..."
         putStrLn ""
         threadDelay 13000000
         putStrLn "MARIA:"
         putStr "♫ Oh "
         hFlush stdout
         threadDelay 500000
         putStr "my "
         hFlush stdout
         threadDelay 500000
         putStrLn "hero,"
         threadDelay 3000000
         putStrLn "so far away now."
         threadDelay 2000000
         putStrLn "Will I ever see your smile?"
  doIONow putStrLn "Scheduling complete. Enjoy!"
  sync

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> devices
    (number : []) -> do
      let device = read number in execSinger opera device
      

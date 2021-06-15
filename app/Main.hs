{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Main where

import Lib

import Control.Concurrent
import Control.Monad.Trans
import Euterpea
import System.Environment

import HSoM.Examples.MUIExamples2

testSinger :: Singer ()
testSinger = do
  singMe (22 :: Int) 
  singMe (33 :: Int) 
  celes <- lift $ importFile "example/ff3celes.mid"
  case celes of
    Left err -> lift $ print err
    Right mid -> singMe (fromMidi mid)
  lift $ threadDelay 100000000
  -- sync

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> devices
    (number : []) -> do
      let device = read number in execSinger testSinger device
      
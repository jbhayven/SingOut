{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Main where

import Singer

import Control.Concurrent
import Control.Monad.Trans
import Euterpea
import System.Environment
import System.IO

import HSoM.Examples.MUIExamples2

intertwine :: Singer ()
intertwine = do
  forkSinger $ loopSinger $ sing "si"
  forkSinger $ loopSinger $ sing "do"
  doIONow threadDelay 10000000 -- limit execution to 10s, otherwise the program will evenutally consume all memory

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> devices
    (number : []) -> do
      let device = read number in execSinger intertwine device
      

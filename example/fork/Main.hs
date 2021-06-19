{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Main where

import Singer

import Control.Concurrent
import Control.Monad.Trans
import Euterpea
import System.Environment
import System.IO

intertwine :: Singer ()
intertwine = do
  forkSinger $ loopSinger $ sing "si" >> doIONow threadDelay 1000000 -- delay needed to avoid starvation
  loopSinger $ sing "do" >> doIONow threadDelay 1000000

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> devices
    (number : []) -> do
      let device = read number in execSinger intertwine device
      

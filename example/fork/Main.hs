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
  forkSinger $ loopSinger $ (sing "si" >> sync)
  forkSinger $ loopSinger $ (sing "do" >> sync)
  doIONow threadDelay (10 * 10^6)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> devices
    (number : []) -> do
      let device = read number in execSinger intertwine device
      

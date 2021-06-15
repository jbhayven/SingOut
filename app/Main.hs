module Main where

import Lib

import Euterpea
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> devices
    (number : []) -> do
      let device = read number in execSinger testSinger device
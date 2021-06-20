{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Main where

import Singer

import System.Environment
import System.IO

computeFib :: Int -> Int
computeFib 0 = 0
computeFib 1 = 1
computeFib n = (computeFib (n-1)) + (computeFib (n-2))

singAllFibs :: Int -> Singer ()
singAllFibs 0 = pure ()
singAllFibs i = do
  singAllFibs (i-1)
  let fib = computeFib i 
  doIONow putStrLn ("Computed: " ++ show fib)
  doIO putStrLn ("Now playing: " ++ show fib)
  sing fib

fibonacciSinger :: Int -> Singer ()
fibonacciSinger n = do
  setVoice HammondOrgan
  setRelativeTempo 1.5

  singAllFibs n
  sync

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> devices
    (number : []) -> do
      let device = read number in execSinger (fibonacciSinger 30) device
      

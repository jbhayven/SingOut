{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Main where

import Singer

import Control.Concurrent
import Control.Monad
import System.Environment
import System.IO

computeFib :: Int -> Singer Int
computeFib 0 = pure 0
computeFib 1 = pure 1
computeFib n = do
  left <- computeFib (n-1)
  right <- computeFib (n-2)
  when (n > 10) $ liftIO yield -- do not starve the scheduler
  pure (left + right)

singAllFibs :: Int -> Singer ()
singAllFibs 0 = pure ()
singAllFibs i = do
  singAllFibs (i-1)
  fib <- computeFib i 
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
      let device = read number in execSinger (fibonacciSinger 50) device
      

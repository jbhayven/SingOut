{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Main where

import Singer

import System.Environment
import System.IO

redomifa :: Singer ()
redomifa = do
  setVoice Shakuhachi
  sing "La redomifa"

  setRelativeTempo 1.4
  transposeVoice 12
  sing $ "Remi sollafasol dosoldola la redomifa" 
  sing $ "Faremi redo lafala solsidola dofasolmi fasi" 
  sing $ "Mimidomi domi solsol fasimila dofa solfasoldo" 
  sing $ "Dofa faremi redo dore milasi domi misilala"

  sync

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> devices
    (number : []) -> do
      let device = read number in execSinger redomifa device
      

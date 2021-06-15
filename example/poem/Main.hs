{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Main where

import Lib

import Control.Concurrent
import Control.Monad.Trans
import Euterpea
import System.Environment
import System.IO

import HSoM.Examples.MUIExamples2

redomifa :: Singer ()
redomifa = do
  sing "La redomifa"
  sing $ "Remi sollafasol dosoldola la redomifa " ++
       "Faremi redo lafala solsidola dofasolmi fasi " ++
       "Mimidomi domi solsol fasimila dofa solfasoldo " ++
       "Dofa faremi redo dore milasi domi misilala "
  sync

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> devices
    (number : []) -> do
      let device = read number in execSinger redomifa device
      
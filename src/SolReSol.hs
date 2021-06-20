{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module SolReSol (SolReSol(toMusic)) where

import Data.Char
import Data.List
import Data.List.Split
import Text.Read

import Euterpea

-- SolReSol representations

type SRSChar = Int
type SRSWord = [SRSChar]

beat :: Dur
beat = qn

noteDo :: Music Pitch
noteDo = c 4 beat
noteRe = d 4 beat
noteMi = e 4 beat
noteFa = f 4 beat
noteSol = g 4 beat
noteLa = a 4 beat
noteSi = b 4 beat

sounds :: [Music Pitch]
sounds = [noteDo, noteRe, noteMi, noteFa, noteSol, noteLa, noteSi]

toNote :: SRSChar -> Music Pitch
toNote char 
  | char < 1 = undefined
  | char > 7 = undefined
  | otherwise = sounds !! (char - 1)

wordToMusic :: SRSWord -> Music Pitch
wordToMusic w 
  | length w == 0 = rest en
  | otherwise     = line $ map toNote w

class (SolReSol a) where
  toSRS     :: a -> [SRSWord]

  toMusic   :: a -> Music Pitch
  toMusic x = line $ intersperse (rest en) $ map (wordToMusic) $ toSRS x

-- Instance (SolReSol Bool) :: begin

instance (SolReSol Bool) where
  toSRS True = [[1, 4, 5, 6]]
  toSRS False = map reverse (toSRS True) -- this is how opposites in solresol work

-- Instance (SolReSol Bool) :: end

-- Instance (SolReSol Int) :: begin

bignums :: [(Int, SRSWord)]
bignums = [(1000 * 1000 * 1000, [4, 6, 6]),
          (1000 * 1000, [4, 5, 5]),
          (1000, [4, 3, 3]),
          (100, [4, 2, 2]),
          (80, [4, 1, 1]),
          (60, [4, 4, 7]),
          (50, [4, 4, 6]),
          (40, [4, 4, 5]),
          (30, [4, 4, 3]),
          (20, [4, 4, 2]),
          (19, [4, 4, 1])]

promote :: Int -> Int -> Int
promote a b = if a > b then b else b + 1

finalizeSmall :: Int -> SRSWord
finalizeSmall a
  | a <= 6   = 
    let r = promote 2 a in
      [2, r, r]
  | a <= 12  = 
    let r = promote 3 (a-6) in
      [3, 3, r]
  | otherwise = 
    let r = promote 3 (a-12) in
      [3, r, r]

process :: Int -> [(Int, SRSWord)] -> SRSWord
process 0 _  = []
process a [] = finalizeSmall a
process a ((val, word):t)  
  | a >= val = 
    let left  = a `div` val
        right = a `mod` val
    in 
    let left_part  = if left > 1 then process left t else []
        middle     = word
        right_part = process right t
    in left_part ++ middle ++ right_part
  | a < val  = process a t

peel :: Int -> SRSWord
peel a = process a bignums

instance (SolReSol Int) where 
  toSRS a
    | a == 0 = [[5, 1]]
    | a < 0  = [[4, 3, 7, 7] ++ peel (-a)] -- famisisi = 'negative'
    | a > 0  = [peel a]

-- Instance (SolReSol Int) :: end

-- Instance (SolReSol String) :: begin

instance (SolReSol String) where
  toSRS str = 
    let words = splitOn " " (map toLower str)  
    in concat $ map convert words
    where 
      convert :: String -> [SRSWord]
      convert str = case (readMaybe str :: Maybe Int) of
        Just num -> toSRS num
        Nothing -> [convertAux str values]

      values = [("do", 1), ("re", 2), ("mi", 3), 
                ("fa", 4), ("sol", 5), ("la", 6), ("si", 7)]

      convertAux :: String -> [(String, Int)] -> SRSWord
      convertAux "" [] = []           -- success
      convertAux _  [] = undefined    -- invalid string
      convertAux str ((name, val):t) =  
        let (left, right) = splitAt (length name) str
        in if (left == name) then 
          val : (convertAux right values) -- match found
        else
          convertAux str t                -- continue searching

-- Instance (SolReSol String) :: end


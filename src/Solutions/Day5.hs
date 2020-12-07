{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Solutions.Day5  where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List hiding (partition)
import Data.Maybe
import Solutions.Day4 (both)

-- F
-- ####
-- ####
-- ####
-- ####

-- FRLR

-- Parse input strings, turning them into space-partitioning functions

floyd :: [a] -> [a]
floyd = join go 
    where go ttl [] = ttl
          go ttl [h] = ttl
          go (t:ts) (h:_:hs) = go ts hs

-- | @getLastHalf gets the last half of a list
getLastHalf, getFirstHalf :: [a] -> [a]
getLastHalf = floyd
getFirstHalf = ap (zipWith const) floyd

partition :: Char -> ([a] -> [a])
partition = \case
    'F' -> getFirstHalf
    'L' -> getFirstHalf
    'B' -> getLastHalf
    'R' -> getLastHalf

-- parseInput :: String -> [Int]
  -- String ->  [Str]           Str->(Str, Str) -> (Str,Str)->(Int,Int)  
parseInput :: String -> [(Int, Int)]
parseInput = lines >>> map (splitAt 7 >>> frontToBack *** leftToRight )

parseInp_ :: String -> [Int]
parseInp_ = parseInput >>> map (first (* 8) >>> uncurry (+))

day5Pt1 :: String -> Int
day5Pt1 = parseInp_ >>> maximum

day5Pt2 :: [Int] -> [Int]
day5Pt2 = flip filter [1..1000] . (not .) . flip elem 
-- filter (both (>= 100) (<= 900))
    -- [x | x <- [1..1000], not (x `elem` z)]
binSpc :: [Int] -> String -> Int
binSpc xs = foldl (flip partition) xs >>> head

frontToBack :: String -> Int
frontToBack = binSpc [0..127]

leftToRight :: String ->Int
leftToRight = binSpc [0..7]

-- generate [0..127]
              -- apply partition successively at each char
                         
-- (0, 127)
-- F (0, 63)
-- R
-- F
-- F
-- R
-- F
-- R

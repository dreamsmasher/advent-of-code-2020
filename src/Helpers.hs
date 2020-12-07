{-# LANGUAGE TupleSections #-}
module Helpers (
    module Control.Arrow,
    module Control.Applicative,
    module Control.Monad,
    module Data.Maybe,
    module Data.List,
    aocGreeter,
    splitComma,
    (?),
    groupLines,
    replace,
    counter,
    both,
    is,
    getGroups
) where

import Data.List.Split
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Map as M


(?) :: Bool -> a -> a -> a
(?) True = const
(?) False = const id

replace :: Eq a => a -> a -> [a] -> [a]
replace o n = map (\x -> (x == o) ? n $ x)

counter :: (Ord a) => [a] -> M.Map a Int
counter = (( , 1) <$>) >>> M.fromListWith (+)

-- for problems like 2sum
splitComma :: [Char] -> [[Char]]
splitComma = splitOn ","

-- for problems like papers please
groupLines :: [Char] -> [[[Char]]]
groupLines = map lines . splitOn "\n\n"

-- for problems like airplane seats
getGroups :: [Char] -> [[Char]]
getGroups = map (filter (/= '\n')) . splitOn "\n\n"

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both a b = liftA2 (&&) a b 

is :: Eq b => (a -> b) -> b -> a -> Bool
is f b a = (f a == b)

aocGreeter :: String
aocGreeter = "Merry chrimbus!!!"


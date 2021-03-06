module Solutions.Day6 where

import Data.Map (Map)
import qualified Data.Map as M
import Data.List.Split ( splitOn )
import Helpers ( (>>>), counter )

getIntersections :: (String -> Map Char Int) -> String -> Int
getIntersections f = splitOn "\n\n" >>> map f >>> foldr ((+) . M.size) 0

day6Pt1 :: String -> Int
day6Pt1 = getIntersections $ filter (/= '\n') >>> counter

day6Pt2 :: String -> Int
day6Pt2 = getIntersections $ lines >>> map counter >>> foldr1 M.intersection  


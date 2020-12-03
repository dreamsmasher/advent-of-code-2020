module Day3 where

import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.List.Split
import Data.Function

(?) :: Bool -> a -> a -> a
(?) True = const
(?) False = const id

inp2Int :: [Char] -> [[Int]]
inp2Int =  lines >>> fmap (cycle . fmap (\c -> ((c == '.') ? 0 $ 1)))

tobSlope :: Int -> [[Int]] -> [[Int]]
tobSlope _ [] = []
tobSlope run (x:xs) = x : tobSlope run (drop run <$> xs)

slide run = (iterate (fmap (drop run) >>> tail) >>= zipWith const) >>> fmap head
    -- liftA2 (zipWith const) (iterate (fmap (drop run) >>> tail)) id >>> fmap head

stepSlope :: [[Int]] -> Int -> Int -> Int
stepSlope xs run rise = xs & (chunksOf rise >>> map head >>> tobSlope run >>> map head >>> sum)

day3Pt1 :: [Char] -> Int
day3Pt1 = inp2Int >>> flip (flip stepSlope 3) 1

day3Pt2 :: [Char] -> Int
day3Pt2 = inp2Int >>> pure . uncurry . stepSlope >>> flip ap [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)] >>> product

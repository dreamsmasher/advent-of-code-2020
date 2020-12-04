    module Day3 where
    
    import Control.Arrow
    import Control.Monad
    import Control.Applicative
    import Data.List.Split
    import Data.Function
    
    (?) :: Bool -> a -> a -> a
    (?) True = const
    (?) False = const id
    
    format :: [Char] -> [[Int]]
    format =  lines >>> fmap (cycle . fmap (fromEnum . (== '#')))
    
    meander :: Int -> [[Int]] -> [[Int]]
    meander _ [] = []
    meander run (x:xs) = x : meander run (drop run <$> xs)
    
    slide :: Int -> [[Int]] -> [[Int]]
    slide run = (iterate (fmap (drop run) >>> tail) >>= zipWith const) >>> fmap head
    
    stepSlope :: [[Int]] -> Int -> Int -> Int
    stepSlope xs run rise = xs & (chunksOf rise >>> map head >>> slide run >>> map head >>> sum)
    
    day3Pt1 :: [Char] -> Int
    day3Pt1 = format >>> flip (flip stepSlope 3) 1
    
    day3Pt2 :: [Char] -> Int
    day3Pt2 = format >>> uncurry . stepSlope >>> (<$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]) >>> product

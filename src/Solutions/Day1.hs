module Solutions.Day1 where
import Control.Monad
import Control.Arrow
    
twoSum :: Int -> [Int] -> [Int]
twoSum n xs = [a * b | a <- xs, b <- xs, a + b == n]

twoSum' :: Int -> [Int] -> Int
twoSum' n = join (liftM2 (,)) >>> filter ((== n) . uncurry (+)) >>> head >>> (uncurry (*))

day1Pt1 :: Int -> [Int] -> String
day1Pt1 n = show . twoSum' n

threeSum :: Int -> [Int] -> [Int]
threeSum n xs = [(a * b * c) | a <- xs, b <- xs, c <- xs, a + b + c == n]

threeSum' :: Int -> [Int] -> Int
threeSum' n = join (join (liftM3 (,,))) >>> filter ((== n) . ap3 (+)) >>> head >>> (ap3 (*))
    where ap3 f (a, b, c)  = a `f` b `f` c


day1Pt2 :: Int -> [Int] -> String
day1Pt2 n = show . threeSum' n
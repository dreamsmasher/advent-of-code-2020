module Day2 where
-- import Text.Regex.TDFA
import Text.Regex
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Function
import Data.List
import Data.List.Split

import Data.Map (Map)
import qualified Data.Map as M

sol :: String -> Bool
sol = words >>> \[n, ch, p] -> checkCt n ch p

counter :: (Ord a) => [a] -> Map a Int
counter = (`zip` repeat 1) >>> M.fromListWith (+)
    
checkCt n c p = let (a, b, ch) = mkTest n c
                 in M.lookup ch (counter p) & (fromMaybe 0 >>> liftA2 (&&) (>= a) (<= b))

mkTest :: String -> String -> (Int, Int, Char)
mkTest nums ch = let [c, ':'] = ch
                     [a, b] = read <$> splitOn "-" nums
                  in (a, b, c)

validTob :: (Int, Int, Char) -> String -> Bool
validTob (l, r, c) = liftA2 (/=) (genCmp c $ pred l) (genCmp c $ pred r)

genCmp i 0 (x:xs) = i == x
genCmp i n [] = False
genCmp i n (x:xs) = genCmp i (pred n) xs

toboggan = words >>> \[n, ch, p] -> (validTob $ mkTest n ch) p

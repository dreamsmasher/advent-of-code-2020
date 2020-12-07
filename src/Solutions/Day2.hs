module Solutions.Day2 (day2Pt1, day2Pt2) where
-- import Text.Regex.TDFA
import Data.Function
import Data.List.Split
import Helpers

import qualified Data.Map as M


    
checkCt :: [Char] -> [Char] -> [Char] -> Bool
checkCt n c p = let (a, b, ch) = mkTest n c
                 in M.lookup ch (counter p) & (fromMaybe 0 >>> liftA2 (&&) (>= a) (<= b))

mkTest :: String -> String -> (Int, Int, Char)
mkTest nums ch = let [c, ':'] = ch
                     [a, b] = read <$> splitOn "-" nums
                  in (a, b, c)

validTob :: (Int, Int, Char) -> String -> Bool
validTob (l, r, c) = liftA2 (/=) (genCmp c $ pred l) (genCmp c $ pred r)

genCmp :: (Num t1, Eq t1, Eq t2, Enum t1) => t2 -> t1 -> [t2] -> Bool
genCmp i 0 (x:_) = i == x
genCmp _ _ [] = False
genCmp i n (_:xs) = genCmp i (pred n) xs


sol :: (String -> String -> String -> c) -> String -> c
sol f = words >>> \[n, ch, p] -> (f n ch) p

day2Pt1 :: String -> Bool
day2Pt1 = sol checkCt
   -- words >>> \[n, ch, p] -> checkCt n ch p

day2Pt2 :: String -> Bool
day2Pt2 = sol ((validTob .) . mkTest)
   --words >>> \[n, ch, p] -> (validTob $ mkTest n ch) p

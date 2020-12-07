module Solutions.Day7 where

import Helpers
import Data.Map (Map, (!))
import qualified Data.Map as M

parseBag :: String -> (String, [(String, Int)])
parseBag = splitOn " bags contain" >>> \[c, s] -> (c, ("no other" `isInfixOf` s? [] $ (map parseDesc $ splitComma s)))

parseDesc :: String -> (String, Int)
parseDesc = words >>> filter (not . ("bag" `isPrefixOf`)) >>> (tail &&& head) >>> (unwords *** read) 
    -- \(n:bs) -> (unwords bs, read n))

fromInput :: String -> Graph
fromInput = lines >>> map parseBag >>> M.fromList

type Graph = Map String [(String, Int)]

walk :: Graph -> String -> Bool
walk g b = "shiny gold" `elem` kids || any (walk g) kids
    where kids = map fst $ (g ! b)

day7Pt1 :: String -> Int
day7Pt1 = fromInput >>> liftA2 filter walk (map fst . M.toList) >>> length

innerBags :: Graph -> String -> Int
innerBags g b = succ (sum $ uncurry ((*) . innerBags g) <$> (g ! b))
               
day7Pt2 :: String -> Int
day7Pt2 = fromInput >>> (pred . flip innerBags "shiny gold")
    -- pred because a shiny gold bag can't contain itself
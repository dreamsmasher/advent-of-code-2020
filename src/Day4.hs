{-# LANGUAGE LambdaCase #-}
module Day4 where

import Control.Applicative
import Control.Arrow
import Data.List
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

-- this is absolute spaghetti code but I was in a rush to finish 
-- still didn't make the leaderboard :(
-- didn't feel like dealing with Parsec today

data Passport = Byr Int 
              | Iyr Int
              | Eyr Int
              | Hgt String
              | Hcl String
              | Ecl String
              | Cid String
              | Pid String deriving (Eq, Ord, Show)
              
-- convenience functions that should be in the prelude
is :: Eq b => (a -> b) -> b -> a -> Bool
is f b a = (f a == b)

(?) :: Bool -> a -> a -> a
(?) True = const
(?) False = const id

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both = liftA2 (&&)

hx :: String
hx = ['0'..'9'] ++ ['a'..'f']

validHex :: String -> Bool
validHex = \case
    '#':xs -> both (length `is` 6) (all (`elem` hx)) xs
    _      -> False

validateP :: Passport -> Bool
validateP = \case
    Byr n -> between 1920 2002 n
    Iyr n -> between 2010 2020 n
    Eyr n -> between 2020 2030 n
    Hgt s -> cnv ("cm" `isSuffixOf` s ? (150,193) $ (59, 76)) s
                where cnv (a, b) = ((init . init) >>> tryNum >>> between a b)
    Hcl s -> validHex s
    Ecl s -> any (== s) (words "amb blu brn gry grn hzl oth")
    Pid s -> both (all isDigit) (length `is` 9)  s
    Cid _ -> True -- optional

tryNum :: String -> Int -- guarantee that false match will be OOB, w/e
tryNum = fromMaybe 0 . readMaybe 

parsePort :: String -> Passport
parsePort = toPair >>> uncurry (\case "byr" -> Byr . tryNum
                                      "iyr" -> Iyr . tryNum
               {- very ugly -}        "eyr" -> Eyr . tryNum
                                      "hgt" -> Hgt
                                      "hcl" -> Hcl
                                      "ecl" -> Ecl
                                      "pid" -> Pid
                                      "cid" -> Cid)

validInfo :: [String] -> Bool
validInfo = all (validateP . parsePort)

between :: Int -> Int -> Int -> Bool
between l r = both (>= l) (<= r)

toPair :: String -> (String, String)
toPair = splitOn ":" >>> \[a, b] -> (a, b)

validate :: [String] -> Bool
validate =  flip elem >>> all >>> ($ words "byr iyr eyr hgt hcl ecl pid") 

validFields :: [String] -> Bool
validFields = map (fst . toPair) >>> validate

replace :: Eq a => a -> a -> ([a] -> [a])
replace a b = map ((== a) >>= (? b))

sol :: ([String] -> Bool) -> String -> Int
sol f = splitOn "\n\n" >>> filter (not . null) >>> map (replace '\n' ' ') >>> filter (f . words) >>> length

day4Pt1, day4Pt2  :: String -> Int
day4Pt1 = sol validFields
day4Pt2 = sol (both validFields validInfo)

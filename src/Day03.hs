module Day03 (main) where

import Data.Char (isDigit)
import Data.List (findIndices, groupBy)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

-- Types --

data Number = Number
  { value :: Int,
    maskFrom :: Int,
    maskTo :: Int
  }
  deriving (Eq, Show)

overlaps :: Number -> Int -> Bool
overlaps (Number _ f t) n = n >= f && n <= t

overlapsAny :: Number -> [Int] -> Bool
overlapsAny n = any (overlaps n)

numbersOverlap :: [Number] -> Int -> [Number]
numbersOverlap nums m = filter (`overlaps` m) nums

-- Parsing --

readMaybeNumber :: (String, (Int, Int)) -> Maybe Number
readMaybeNumber (val, (f, t)) = if all isDigit val then Just (Number (read val) (f - 1) t) else Nothing

findNumbers :: String -> [Number]
findNumbers s = mapMaybe readMaybeNumber maybeNumbers
  where
    maybeNumbers = zip groups (zip (0 : cumulativeLength) cumulativeLength)
    cumulativeLength = scanl1 (+) $ map length groups
    groups = groupBy (\a b -> isDigit a && isDigit b || a == b) s

findSymbols :: String -> [Int]
findSymbols = findIndices (`notElem` ".0123456789")

-- Helper functions --

-- divvy n m -> splits a list into sublists, each of length n and starting with offset m from the previous one
divvy :: Int -> Int -> [a] -> [[a]]
divvy _ _ [] = []
divvy n m lst = b : divvy n m as
  where
    b = take n lst
    as = drop m lst

-- Part 1 --

filterNumbers :: ([Number], [Int]) -> [Number]
filterNumbers ([], _) = []
filterNumbers (n : ns, m) = if n `overlapsAny` m then n : rst else rst
  where
    rst = filterNumbers (ns, m)

part1 :: [[Number]] -> [[Int]] -> String
part1 numbers symbols = show $ sum $ map value $ concatMap filterNumbers $ zip numbers symbolMap
  where
    symbolMap = map concat $ divvy 3 1 ([] : symbols)

-- Part 2 --

gearRatio :: [Number] -> Maybe Int
gearRatio [Number v1 _ _, Number v2 _ _] = Just (v1 * v2)
gearRatio _ = Nothing

filterGearsRatios :: ([Int], [Number]) -> [Int]
filterGearsRatios (g, nums) = mapMaybe (gearRatio . numbersOverlap nums) g

part2 :: [[Number]] -> [[Int]] -> String
part2 numbers symbols = show $ sum $ concatMap filterGearsRatios $ zip symbols numbersMap
  where
    numbersMap = map concat $ divvy 3 1 ([] : numbers)

-- Main --

main :: IO ()
main = do
  [dataFile] <- getArgs
  fileContents <- readFile dataFile
  let numbers = map findNumbers (lines fileContents)
      symbols = map findSymbols $ lines fileContents
   in putStrLn ("part1 " ++ part1 numbers symbols) >> putStrLn ("part2 " ++ part2 numbers symbols)
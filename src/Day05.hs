module Day05 (main) where

import Control.Monad (void)
import System.Environment (getArgs)
import Text.Parsec (char, digit, endOfLine, letter, many1, parse, sepBy, sepEndBy, space, spaces, string)
import Text.Parsec.String (Parser)

-- Types --

data Range = Range Int Int Int deriving (Show)

-- Parser --

int :: Parser Int
int = read <$> many1 digit

seeds :: Parser [Int]
seeds = string "seeds: " *> int `sepBy` char ' '

range :: Parser Range
range = do
  f <- int
  void space
  t <- int
  void space
  r <- int
  pure $ Range f t r

parseRangeMaps :: Parser [Range]
parseRangeMaps = do
  void $ many1 letter
  void $ string "-to-"
  void $ many1 letter
  void $ string " map:\n"
  range `sepEndBy` endOfLine

parseInput :: Parser ([Int], [[Range]])
parseInput = do
  s <- seeds
  spaces
  m <- parseRangeMaps `sepBy` many1 endOfLine
  pure (s, m)

-- Part 1 --

mapByRange :: Int -> [Range] -> Int
mapByRange i [] = i
mapByRange i (Range t f r : rs) = let d = i - f in if d >= 0 && d < r then t + d else mapByRange i rs

mapRM :: Int -> [[Range]] -> Int
mapRM i [rs] = mapByRange i rs
mapRM i (rs : rms) = mapRM (mapByRange i rs) rms

minimumLocation :: ([Int], [[Range]]) -> Int
minimumLocation (s, r) = minimum $ map go s
  where
    go i = mapRM i r

part1 :: ([Int], [[Range]]) -> String
part1 = show . minimumLocation

-- Part 2 --

expandSeeds :: [Int] -> [Int]
expandSeeds [] = []
expandSeeds [_] = []
expandSeeds (start : num : ss) = take num (iterate (+ 1) start) ++ expandSeeds ss

part2 :: ([Int], [[Range]]) -> String
part2 (s, r) = show $ minimumLocation (expandSeeds s, r)

-- Main --

main :: IO ()
main = do
  [dataFile] <- getArgs
  fileContents <- readFile dataFile
  case parse parseInput "" fileContents of
    Left err -> print err
    Right s -> putStrLn ("Part1: " ++ part1 s) >> putStrLn ("Part2: " ++ part2 s)
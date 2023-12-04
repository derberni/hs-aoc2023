module Day04 (main) where

import Control.Monad (void)
import System.Environment (getArgs)
import Text.Parsec (char, digit, endOfLine, many1, optional, parse, sepBy, sepEndBy, spaces, string)
import Text.Parsec.String (Parser)

-- Parser --

numbers :: Parser [Int]
numbers = do
  spaces
  n <- (optional (char ' ') *> many1 digit) `sepEndBy` char ' '
  pure $ map read n

card :: Parser ([Int], [Int])
card = do
  void $ string "Card"
  spaces
  void $ many1 digit
  void $ char ':'
  spaces
  w <- numbers
  void $ char '|'
  n <- numbers
  pure (w, n)

-- Calculating winning cards --

countWins :: ([Int], [Int]) -> Int
countWins (w, n) = sum $ map (fromEnum . (`elem` w)) n

-- Part 1 --

pow :: Int -> Int
pow 0 = 0
pow a = 2 ^ (a - 1)

part1 :: [([Int], [Int])] -> String
part1 l = show $ sum $ map (pow . countWins) l

-- Part 2 --

winCards :: [Int] -> [Int]
winCards [] = []
winCards (x : xs) = 1 + sum (take x rst) : rst
  where
    rst = winCards xs

part2 :: [([Int], [Int])] -> String
part2 l = show $ sum $ winCards $ map countWins l

-- Main --

main :: IO ()
main = do
  [dataFile] <- getArgs
  fileContents <- readFile dataFile
  case parse (card `sepBy` endOfLine) "" fileContents of
    Left err -> print err
    Right cards -> putStrLn ("Part1: " ++ part1 cards) >> putStrLn ("Part2: " ++ part2 cards)
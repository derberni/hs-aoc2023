module Day01 (main) where

import Control.Monad (void)
import Data.Char (isLetter)
import System.Environment (getArgs)
import Text.Parsec (alphaNum, digit, many1, parse, string, try, (<|>))
import Text.Parsec.String (Parser)

firstDigit :: String -> Char
firstDigit s = head $ dropWhile isLetter s

extractNumber :: String -> String
extractNumber s = [a, b]
  where
    a = firstDigit s
    b = firstDigit $ reverse s

toInt :: String -> Int
toInt = read

part1 :: String -> String
part1 s = show $ sum $ map (toInt . extractNumber) $ lines s

one :: Parser Int
one = do
  void $ string "one"
  pure 1

two :: Parser Int
two = do
  void $ string "two"
  pure 2

three :: Parser Int
three = do
  void $ string "three"
  pure 3

four :: Parser Int
four = do
  void $ string "four"
  pure 4

five :: Parser Int
five = do
  void $ string "five"
  pure 5

six :: Parser Int
six = do
  void $ string "six"
  pure 6

seven :: Parser Int
seven = do
  void $ string "seven"
  pure 7

eight :: Parser Int
eight = do
  void $ string "eight"
  pure 8

nine :: Parser Int
nine = do
  void $ string "nine"
  pure 9

oneR :: Parser Int
oneR = do
  void $ string "eno"
  pure 1

twoR :: Parser Int
twoR = do
  void $ string "owt"
  pure 2

threeR :: Parser Int
threeR = do
  void $ string "eerht"
  pure 3

fourR :: Parser Int
fourR = do
  void $ string "ruof"
  pure 4

fiveR :: Parser Int
fiveR = do
  void $ string "evif"
  pure 5

sixR :: Parser Int
sixR = do
  void $ string "xis"
  pure 6

sevenR :: Parser Int
sevenR = do
  void $ string "neves"
  pure 7

eightR :: Parser Int
eightR = do
  void $ string "thgie"
  pure 8

nineR :: Parser Int
nineR = do
  void $ string "enin"
  pure 9

int :: Parser Int
int = do
  d <- digit
  pure $ read [d]

other :: Parser Int
other = do
  void alphaNum
  pure 0

calibrationValue :: Parser Int
calibrationValue = int <|> try one <|> try two <|> try three <|> try four <|> try five <|> try six <|> try seven <|> try eight <|> try nine <|> other

calibrationValueR :: Parser Int
calibrationValueR = int <|> try oneR <|> try twoR <|> try threeR <|> try fourR <|> try fiveR <|> try sixR <|> try sevenR <|> try eightR <|> try nineR <|> other

firstLast :: ([Int], [Int]) -> Int
firstLast (l, r) = a * 10 + b
  where
    a = head $ dropWhile (== 0) l
    b = head $ dropWhile (== 0) r

parseLine :: String -> ([Int], [Int])
parseLine s = case (parse (many1 calibrationValue) "" s, parse (many1 calibrationValueR) "" (reverse s)) of
  (Right f, Right l) -> (f, l)
  _ -> ([], [])

part2 :: String -> String
part2 s = show $ sum $ map (firstLast . parseLine) (lines s)

main :: IO ()
main = do
  [dataFile] <- getArgs
  fileContents <- readFile dataFile
  putStrLn $ "part1 " ++ part1 fileContents
  putStrLn $ "part2 " ++ part2 fileContents
module Day07 (main) where

import Data.Char (isDigit)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (group, sort, sortBy)
import System.Environment (getArgs)
import Text.Parsec (alphaNum, digit, endOfLine, many1, parse, sepEndBy, spaces)
import Text.Parsec.String (Parser)

type Cards = [Int]

data Hand = Five Cards | Four Cards | FullHouse Cards | Three Cards | TwoPair Cards | OnePair Cards | HighCard Cards deriving (Eq, Show)

instance Ord Hand where
  Five c1 `compare` Five c2 = c1 `compare` c2
  Five _ `compare` _ = GT
  Four c1 `compare` Four c2 = c1 `compare` c2
  Four _ `compare` Five _ = LT
  Four _ `compare` _ = GT
  FullHouse c1 `compare` FullHouse c2 = c1 `compare` c2
  FullHouse _ `compare` Five _ = LT
  FullHouse _ `compare` Four _ = LT
  FullHouse _ `compare` _ = GT
  Three c1 `compare` Three c2 = c1 `compare` c2
  Three _ `compare` Five _ = LT
  Three _ `compare` Four _ = LT
  Three _ `compare` FullHouse _ = LT
  Three _ `compare` _ = GT
  TwoPair c1 `compare` TwoPair c2 = c1 `compare` c2
  TwoPair _ `compare` OnePair _ = GT
  TwoPair _ `compare` HighCard _ = GT
  TwoPair _ `compare` _ = LT
  OnePair c1 `compare` OnePair c2 = c1 `compare` c2
  OnePair _ `compare` HighCard _ = GT
  OnePair _ `compare` _ = LT
  HighCard c1 `compare` HighCard c2 = c1 `compare` c2
  HighCard _ `compare` _ = LT

type Game = (Hand, Int)

cardValue :: Char -> Int
cardValue 'T' = 10
cardValue 'J' = 11
cardValue 'Q' = 12
cardValue 'K' = 13
cardValue 'A' = 14
cardValue c
  | isDigit c = read [c]
  | otherwise = undefined

cards :: Parser Cards
cards = map cardValue <$> many1 alphaNum

hand :: Cards -> Hand
hand c = case sort $ map length $ group $ sort c of
  [5] -> Five c
  [1, 4] -> Four c
  [2, 3] -> FullHouse c
  [1, 1, 3] -> Three c
  [1, 2, 2] -> TwoPair c
  [1, 1, 1, 2] -> OnePair c
  [1, 1, 1, 1, 1] -> HighCard c
  _ -> undefined

game :: Parser Game
game = do
  c <- cards
  spaces
  b <- many1 digit
  pure (hand c, read b)

toInt64 :: Int -> Int64
toInt64 = fromIntegral

part1 :: [Game] -> String
part1 games = show $ sum $ zipWith (*) [1 ..] $ map (toInt64 . snd) $ sortBy (compare `on` fst) games

main :: IO ()
main = do
  [dataFile] <- getArgs
  fileContents <- readFile dataFile
  case parse (game `sepEndBy` endOfLine) "" fileContents of
    Left err -> print err
    Right games -> putStrLn $ "Part1: " ++ part1 games
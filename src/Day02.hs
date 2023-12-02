module Day02 (main) where

import Control.Monad (void)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Parsec (char, choice, digit, endOfLine, many1, parse, sepBy, spaces, string)
import Text.Parsec.String (Parser)

-- Types --

data Cube = Red Int | Green Int | Blue Int deriving (Eq, Show)

data CubeSet = CubeSet
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Eq, Show)

instance Ord CubeSet where
  (<=) (CubeSet r g b) (CubeSet r2 g2 b2) = r <= r2 && g <= g2 && b <= b2

cubeSetFromList :: [Cube] -> CubeSet
cubeSetFromList [] = CubeSet 0 0 0
cubeSetFromList (Red r : xs) = (cubeSetFromList xs) {red = r}
cubeSetFromList (Green g : xs) = (cubeSetFromList xs) {green = g}
cubeSetFromList (Blue b : xs) = (cubeSetFromList xs) {blue = b}

-- Parser --

line :: Parser (Int, [CubeSet])
line = do
  void $ string "Game "
  d <- many1 digit
  void $ char ':'
  spaces
  s <- gameSet
  pure (read d, map cubeSetFromList s)

cube :: Parser Cube
cube = do
  d <- many1 digit
  spaces
  let count = read d
   in choice [Red count <$ string "red", Green count <$ string "green", Blue count <$ string "blue"]

cubeSet :: Parser [Cube]
cubeSet = cube `sepBy` string ", "

gameSet :: Parser [[Cube]]
gameSet = cubeSet `sepBy` string "; "

-- Part 1 --

limit1 :: CubeSet
limit1 = CubeSet 12 13 14

gameValid :: (Int, [CubeSet]) -> Maybe Int
gameValid (n, l) = if all (<= limit1) l then Just n else Nothing

part1 :: [(Int, [CubeSet])] -> String
part1 g = show $ sum $ mapMaybe gameValid g

-- Part 2 --

maxSet :: [CubeSet] -> CubeSet
maxSet [] = CubeSet 0 0 0
maxSet (CubeSet r g b : xs) = CubeSet (max r r2) (max g g2) (max b b2)
  where
    CubeSet r2 g2 b2 = maxSet xs

power :: CubeSet -> Int
power (CubeSet r g b) = r * g * b

part2 :: [(Int, [CubeSet])] -> String
part2 g = show $ sum $ map (power . maxSet . snd) g

-- Main --

main :: IO ()
main = do
  [dataFile] <- getArgs
  fileContents <- readFile dataFile
  case parse (line `sepBy` endOfLine) "" fileContents of
    Left err -> print err
    Right g -> putStrLn ("part1 " ++ part1 g) >> putStrLn ("part2 " ++ part2 g)
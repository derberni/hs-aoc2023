module Day09 (main) where

import System.Environment (getArgs)

parseInput :: String -> [[Int]]
parseInput s = map (map read . words) (lines s)

derive :: [Int] -> [Int]
derive [] = []
derive [_] = []
derive (x1 : x2 : xs) = x2 - x1 : derive (x2 : xs)

extrapolate :: [Int] -> [Int]
extrapolate l
  | all (== 0) l = 0 : l
  | otherwise = l ++ [last l + (last . extrapolate) l']
  where
    l' = derive l

solve :: [[Int]] -> String
solve l = show $ sum $ map (last . extrapolate) l

main :: IO ()
main = do
  [dataFile] <- getArgs
  fileContents <- readFile dataFile
  let l = parseInput fileContents
  putStrLn ("Part1: " ++ solve l) >> putStrLn ("Part2: " ++ solve (map reverse l))
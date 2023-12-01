module Day01 (main) where

import Data.List (isPrefixOf, tails)
import System.Environment (getArgs)

extractNumbers :: (String -> Int) -> String -> Int
extractNumbers condition s = a * 10 + b
  where
    t = tails s
    a = takeFirst (/= 0) $ map condition t
    b = takeFirst (/= 0) $ map condition $ reverse t

takeFirst :: (a -> Bool) -> [a] -> a
takeFirst b l = head $ dropWhile (not . b) l

part1 :: String -> String
part1 s = show $ sum $ map (extractNumbers matchDigit) $ lines s

matchDigit :: String -> Int
matchDigit s
  | "1" `isPrefixOf` s = 1
  | "2" `isPrefixOf` s = 2
  | "3" `isPrefixOf` s = 3
  | "4" `isPrefixOf` s = 4
  | "5" `isPrefixOf` s = 5
  | "6" `isPrefixOf` s = 6
  | "7" `isPrefixOf` s = 7
  | "8" `isPrefixOf` s = 8
  | "9" `isPrefixOf` s = 9
  | otherwise = 0

matchNumber :: String -> Int
matchNumber s
  | 0 /= d = d
  | "one" `isPrefixOf` s = 1
  | "two" `isPrefixOf` s = 2
  | "three" `isPrefixOf` s = 3
  | "four" `isPrefixOf` s = 4
  | "five" `isPrefixOf` s = 5
  | "six" `isPrefixOf` s = 6
  | "seven" `isPrefixOf` s = 7
  | "eight" `isPrefixOf` s = 8
  | "nine" `isPrefixOf` s = 9
  | otherwise = 0
  where
    d = matchDigit s

part2 :: String -> String
part2 s = show $ sum $ map (extractNumbers matchNumber) $ lines s

main :: IO ()
main = do
  [dataFile] <- getArgs
  fileContents <- readFile dataFile
  putStrLn $ "part1 " ++ part1 fileContents
  putStrLn $ "part2 " ++ part2 fileContents
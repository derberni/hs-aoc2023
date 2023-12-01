module Day02 (main) where

import System.Environment (getArgs)

part1 :: String -> String
part1 s = s

part2 :: String -> String
part2 s = s

main :: IO ()
main = do
  [dataFile] <- getArgs
  fileContents <- readFile dataFile
  putStrLn $ "part1 " ++ part1 fileContents
  putStrLn $ "part2 " ++ part2 fileContents
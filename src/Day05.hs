module Day05 (main) where

import System.Environment (getArgs)

main :: IO ()
main = do
  [dataFile] <- getArgs
  fileContents <- readFile dataFile
  putStrLn fileContents
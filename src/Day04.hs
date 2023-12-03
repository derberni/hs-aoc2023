module Day04 (main) where

import System.Environment (getArgs)

-- Main --

main :: IO ()
main = do
  [dataFile] <- getArgs
  fileContents <- readFile dataFile
  putStrLn fileContents
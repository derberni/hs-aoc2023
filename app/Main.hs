module Main (main) where

import qualified Day1
import System.Environment (getArgs)

dispatch :: [(String, String -> String)]
dispatch =
  [ ("d1p1", Day1.part1),
    ("d1p2", Day1.part2)
  ]

main :: IO ()
main = do
  [task, dataFile] <- getArgs
  let (Just action) = lookup task dispatch
  result <- action <$> readFile dataFile
  putStrLn result

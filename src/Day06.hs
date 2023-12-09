module Day06 (main) where

import Data.List (findIndex)
import System.Environment (getArgs)

parseRaces :: String -> [(Int, Int)]
parseRaces s = zip (map read t) (map read d)
  where
    [t, d] = map (tail . words) $ lines s

parseSingleRace :: String -> (Int, Int)
parseSingleRace s = (read t, read d)
  where
    [t, d] = map (concat . tail . words) $ lines s

distance :: Int -> Int -> Int
distance t mt = t * (mt - t)

findDistances :: (Int, Int) -> Int
findDistances (mt, d) = mt - front - back + 1
  where
    Just front = findIndex (\t -> d < distance t mt) [0 .. mt]
    Just back = findIndex (\t -> d < distance t mt) $ reverse [0 .. mt]

main :: IO ()
main = do
  [dataFile] <- getArgs
  fileContents <- readFile dataFile
  let races = parseRaces fileContents
  putStrLn ("Part1 " ++ show (product $ map findDistances races)) >> putStrLn ("Part2 " ++ show (findDistances $ parseSingleRace fileContents))
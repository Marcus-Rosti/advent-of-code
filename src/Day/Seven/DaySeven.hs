module Day.Seven.DaySeven (day7) where

import Data.List.Split (splitOn)
import Day.FileIO (reader)

cost :: Num a => (a -> a) -> a -> [a] -> a
cost usage n = sum . map (usage . abs . subtract n)

fuelConsumption :: (Num a, Ord a) => (a -> a) -> [a] -> [a] -> a
fuelConsumption usage xs dists = minimum $ map (\t -> cost usage t xs) dists

part1 :: (Num a, Ord a) => [a] -> [a] -> a
part1 = fuelConsumption id

part2 :: (Integral a) => [a] -> [a] -> a
part2 = fuelConsumption (\t -> t * (t + 1) `div` 2)

getCrabs :: String -> [Int]
getCrabs = map read . splitOn ","

day7 :: String -> IO ()
day7 file = do
  contents <- reader file
  let crabs = getCrabs $ head contents
  let dists = [minimum crabs .. maximum crabs]
  print "Day 7"
  print $ part1 crabs dists
  print $ part2 crabs dists
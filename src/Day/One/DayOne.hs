module Day.One.DayOne (day1) where

import Data.List

readLines :: String -> [Int]
readLines = map stringToInt . lines

stringToInt :: String -> Int
stringToInt = read

increasing :: Ord a => [a] -> Bool
increasing [] = True
increasing [_] = True
increasing (a : b : xs) = a < b && increasing (b : xs)

countOfIncreasingValues :: Ord a => [a] -> Int
countOfIncreasingValues = length . filter increasing . slidingWindow 2

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n xs = filter (\t -> length t == n) (transpose (take n (tails xs)))

part1 :: [Int] -> Int
part1 = countOfIncreasingValues

part2 :: [Int] -> Int
part2 = countOfIncreasingValues . map sum . slidingWindow 3

day1 :: String -> IO ()
day1 file = do
  content <- readFile file
  let list = readLines content
  putStrLn "Day 1"
  print $ part1 list
  print $ part2 list

module Day.One.DayOne (day1) where

import Data.List
import Day.Base (Answer (Day), Solution)
import Day.FileIO (reader)

readLines :: [String] -> [Int]
readLines = map stringToInt

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

day1 :: Solution Int Int
day1 file = do
  list <- (fmap readLines . reader) file
  return $ Day 1 (part1 list) (part2 list)
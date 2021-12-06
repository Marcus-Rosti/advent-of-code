module Day.Five.DayFive (day5) where

import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Day.FileIO (reader)

data Point = Point Int Int deriving (Eq, Show, Ord)

makePoint :: (Int, Int) -> Point
makePoint (a, b) = Point a b

data Line = Line Point Point deriving (Eq, Show)

(...) :: Int -> Int -> [Int]
x1 ... x2
  | x1 <= x2 = [x1 .. x2]
  | otherwise = reverse [x2 .. x1]

pointsOnLine :: Line -> [Point]
pointsOnLine (Line (Point x1 y1) (Point x2 y2))
  | x1 == x2 = [Point x1 y | y <- y1 ... y2]
  | y1 == y2 = [Point x y1 | x <- x1 ... x2]
  | otherwise = zipWith (curry makePoint) (x1 ... x2) (y1 ... y2)

count :: Ord a => [a] -> [(a, Int)]
count = map (\t -> (head t, length t)) . (group . sort)

part1 :: [Line] -> Int
part1 xs = length (filter (\(_, x2) -> x2 > 1) (count (concatMap pointsOnLine xs)))

readPoint :: [Int] -> Maybe Point
readPoint [x, y] = Just (Point x y)
readPoint _ = Nothing

readLine :: [String] -> Maybe Line
readLine [a, "->", b] = do
  pointa <- readPoint ((map read . splitOn ",") a)
  pointb <- readPoint ((map read . splitOn ",") b)
  return $ Line pointa pointb
readLine _ = Nothing

readLines :: [String] -> [Line]
readLines = mapMaybe (readLine . words)

day5 :: String -> IO ()
day5 file = do
  contents <- reader file
  let linez = readLines contents
  print "Day 5"
  print $ part1 linez

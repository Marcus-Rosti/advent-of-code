module Day.One.DayOne (day1) where

readLines :: String -> [Int]
readLines = map stringToInt . lines

stringToInt :: String -> Int
stringToInt = read

zipWithSelf :: [a] -> [(a, a)]
zipWithSelf xs = zip (init xs) (tail xs)

filterValues :: Ord a => [(a, a)] -> [(a, a)]
filterValues = filter (uncurry (<))

countOfIncreasingValues :: Ord a => [a] -> Int
countOfIncreasingValues = length . filterValues . zipWithSelf

day1 :: String -> IO ()
day1 file = do
  content <- readFile file
  print $ countOfIncreasingValues (readLines content)

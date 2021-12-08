module Day.Six.DaySix (day6) where

import Data.List.Split (splitOn)
import Day.Base (Answer (Day), Solution)
import Day.FileIO (reader)

data State a = State a a a a a a a a a

sumState :: (Integral a) => State a -> Integer
sumState (State a b c d e f g h i) = sum $ map toInteger [a, b, c, d, e, f, g, h, i]

nextState :: (Integral a) => State a -> State a
nextState (State z o t th f fi si se e) = State o t th f fi si (se + z) e z

countValue :: Eq a => a -> [a] -> Integer
countValue x xs = toInteger $ length (filter (== x) xs)

possibleStates :: [Integer]
possibleStates = [0 .. 8]

toState :: [Integer] -> State Integer
toState xs = makeState [countValue (toInteger x) xs | x <- possibleStates]

makeState :: (Integral a) => [a] -> State a
makeState [z, o, t, th, f, fi, si, se, e] = State z o t th f fi si se e
makeState _ = State 0 0 0 0 0 0 0 0 0

allStates :: (Integral a) => State a -> [State a]
allStates s = s : allStates (nextState s)

stateAtI :: (Integral a) => [State a] -> Int -> State a
stateAtI = (!!)

header :: [String] -> [Int]
header = map read . splitOn "," . head

runProg :: Integral a => [State a] -> Int -> Integer
runProg xs i = sumState $ stateAtI xs i

part1 :: Integral a => [State a] -> Integer
part1 t = runProg t 80

part2 :: Integral a => [State a] -> Integer
part2 t = runProg t 256

day6 :: Solution Integer Integer
day6 file = do
  contents <- header <$> reader file
  let allPossibleStates = allStates (toState (map toInteger contents))
  return $ Day 6 (part1 allPossibleStates) (part2 allPossibleStates)

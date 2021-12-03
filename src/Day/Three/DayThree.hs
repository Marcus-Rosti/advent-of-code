module Day.Three.DayThree where

import Control.Monad (liftM2)
import Data.List
import Day.FileIO (reader)

type Bit = Int

binaryToDecimal :: [Bit] -> Int
binaryToDecimal = foldl (\acc x -> x + acc * 2) 0

mostCommonElement :: Ord a => [a] -> a
mostCommonElement = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

lineToBits :: String -> [Bit]
lineToBits = map (read . (: []))

linesToBits :: [String] -> [[Bit]]
linesToBits = map lineToBits

transposeBits :: [[Bit]] -> [[Bit]]
transposeBits = transpose

mostCommonBits :: [[Bit]] -> [Bit]
mostCommonBits = map mostCommonElement . transposeBits

leastCommonBits :: [[Bit]] -> [Bit]
leastCommonBits = map ((1 +) . ((-1) *)) . mostCommonBits

gammaRate :: [[Bit]] -> Int
gammaRate = binaryToDecimal . mostCommonBits

epsilonRate :: [[Bit]] -> Int
epsilonRate = binaryToDecimal . leastCommonBits

part1 :: [[Bit]] -> Int
part1 = liftM2 (*) gammaRate epsilonRate

day3 :: String -> IO ()
day3 file = do
  contents <- reader file
  let bits = linesToBits contents
  print "Day 3"
  print $ part1 bits
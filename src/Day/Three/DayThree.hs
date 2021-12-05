module Day.Three.DayThree (day3) where

import Control.Monad (liftM2)
import Data.List
import Day.FileIO (reader)

type Bit = Int

flipBit :: Bit -> Bit
flipBit = (1 +) . ((-1) *)

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

leastCommonBitAtI :: Int -> [[Bit]] -> Bit
leastCommonBitAtI = (flipBit .) . mostCommonBitAtI

mostCommonBitAtI :: Int -> [[Bit]] -> Bit
mostCommonBitAtI i bits = mostCommonBits bits !! i

filterBitsAtPosition :: (Int -> [[Bit]] -> Bit) -> Int -> [[Bit]] -> [[Bit]]
filterBitsAtPosition _ (-1) bits = bits
filterBitsAtPosition _ _ [x] = [x]
filterBitsAtPosition cond i bits = filterBitsAtPosition cond (i - 1) (filter (\t -> (t !! correctedIndex) == cond correctedIndex bits) bits)
  where
    correctedIndex = length (head bits) - i - 1

filterBits :: (Int -> [[Bit]] -> Bit) -> [[Bit]] -> [[Bit]]
filterBits cond xs = filterBitsAtPosition cond (len - 1) xs
  where
    len = (length . head) xs

mostCommonBits :: [[Bit]] -> [Bit]
mostCommonBits = map mostCommonElement . transposeBits

leastCommonBits :: [[Bit]] -> [Bit]
leastCommonBits = map flipBit . mostCommonBits

gammaRate :: [[Bit]] -> Int
gammaRate = binaryToDecimal . mostCommonBits

epsilonRate :: [[Bit]] -> Int
epsilonRate = binaryToDecimal . leastCommonBits

oxygenRate :: [[Bit]] -> Int
oxygenRate = binaryToDecimal . head . filterBits mostCommonBitAtI

carbonRate :: [[Bit]] -> Int
carbonRate = binaryToDecimal . head . filterBits leastCommonBitAtI

part1 :: [[Bit]] -> Int
part1 = liftM2 (*) gammaRate epsilonRate

part2 :: [[Bit]] -> Int
part2 = liftM2 (*) oxygenRate carbonRate

day3 :: String -> IO ()
day3 file = do
  contents <- reader file
  let bits = linesToBits contents
  print "Day 3"
  print $ part1 bits
  print $ part2 bits

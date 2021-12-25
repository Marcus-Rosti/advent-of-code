module Day.Day10 (day10) where

import AOC
import Data.List (sort)

parser :: [Char] -> Char -> [Char]
parser ")" _ = ")"
parser "}" _ = "}"
parser "]" _ = "]"
parser ">" _ = ">"
parser xs '(' = '(' : xs
parser xs '{' = '{' : xs
parser xs '[' = '[' : xs
parser xs '<' = '<' : xs
parser ('(' : xs) ')' = xs
parser ('{' : xs) '}' = xs
parser ('[' : xs) ']' = xs
parser ('<' : xs) '>' = xs
parser _ x = [x]

score :: Num p => [Char] -> p
score ")" = 3
score "]" = 57
score "}" = 1197
score ">" = 25137
score _ = 0

parse :: String -> String
parse = foldl parser []

part1 :: [String] -> Int
part1 = sum . map (score . parse)

autocorrect :: Num a => a -> Char -> a
autocorrect x '(' = x * 5 + 1
autocorrect x '[' = x * 5 + 2
autocorrect x '{' = x * 5 + 3
autocorrect x '<' = x * 5 + 4
autocorrect _ _ = 0

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

part2 :: [String] -> Int
part2 = middle . sort . filter (> 0) . map (foldl autocorrect 0 . parse)

day10 :: Solution [String] Int Int
day10 = Solution 10 id part1 part2
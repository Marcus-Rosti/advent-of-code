module Day.Day14 (day14) where

import AOC (Solution (Solution))
import Control.Monad.State (State, evalState, gets, modify, replicateM)
import Data.Char (isSpace)
import Data.List.Split (splitOn)
import Data.Map (Map, elems, fromList, fromListWith, lookup, toList)
import Data.Maybe (fromMaybe, mapMaybe)
import Prelude hiding (lookup)

type Pair = (Char, Char)

type Rules = Map Pair Char

type FreqMap a = Map Pair a

type Polymer a = (FreqMap a, Rules)

trim :: [Char] -> [Char]
trim = f . f
  where
    f = reverse . dropWhile isSpace

toRule :: [Char] -> Maybe (Pair, Char)
toRule [a, b, c] = Just ((a, b), c)
toRule _ = Nothing

toPairs :: Num b => [a] -> [((a, a), b)]
toPairs [] = []
toPairs [_] = []
toPairs (a : b : xs) = ((a, b), 1) : toPairs (b : xs)

parse :: Num a => [[Char]] -> (Map (Char, Char) a, Map Pair Char)
parse linez = (freqs, fromList $ mapMaybe toRule second)
  where
    [start, commands] = splitOn [""] linez
    second = map (concatMap trim . splitOn "->") commands
    freqs = fromListWith (+) $ toPairs $ head start

calc :: (Ord k, Integral v) => Map (k, k) v -> v
calc str = maximum counts - minimum counts
  where
    x = fromListWith (+) $ concat [[(a, cnt), (b, cnt)] | ((a, b), cnt) <- toList str]
    counts = map ((`quot` 2) . (+ 1)) (elems x)

runPair :: Rules -> (Pair, a) -> [(Pair, a)]
runPair rules t@(p@(a, b), i) = fromMaybe [t] result
  where
    result = do
      c <- lookup p rules
      return [((a, c), i), ((c, b), i)]

runPolymer :: Num a => Map Pair a -> Rules -> Map Pair a
runPolymer fm rules = fromListWith (+) $ concatMap (runPair rules) $ toList fm

step :: Integral a => State (Polymer a) a
step = do
  t <- gets (calc . fst)
  modify $ \(string, rules) -> (runPolymer string rules, rules)
  pure t

stepsUpTo :: Integral a => Int -> Polymer a -> [a]
stepsUpTo n = evalState (replicateM (n + 1) step)

part1 :: Polymer Integer -> Integer
part1 = last . stepsUpTo 10

part2 :: Polymer Integer -> Integer
part2 = last . stepsUpTo 40

day14 :: Solution (Polymer Integer) Integer Integer
day14 = Solution 14 parse part1 part2

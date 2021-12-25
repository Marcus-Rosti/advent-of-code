module Day.Day11 (day11) where

import AOC
import Control.Monad.State
import Data.List (elemIndex)
import Data.Map (Map, adjust, filter, fromList, keys, union)
import Data.Maybe (fromJust)
import Prelude hiding (filter)

data Vertex = Vertex Int Int deriving (Eq, Ord, Show)

data Octopus = Energy Int | Flashed

type Grid = Map Vertex Octopus

updateEnergy :: Octopus -> Octopus
updateEnergy (Energy t) = Energy (t + 1)
updateEnergy Flashed = Flashed

excessEnergy :: Int -> Octopus -> Bool
excessEnergy v (Energy n)
  | n > v = True
excessEnergy _ _ = False

reset :: Octopus -> Octopus
reset Flashed = Energy 0
reset e = e

getOctopus :: [[Char]] -> Vertex -> (Vertex, Octopus)
getOctopus g v@(Vertex x y) = (v, Energy (read [(g !! x) !! y]))

neighbors :: Vertex -> [Vertex]
neighbors (Vertex x y) = [Vertex (x + a) (y + b) | a <- [-1 .. 1], b <- [-1 .. 1]]

addEneryAtVertex :: Ord k => (a -> a) -> Map k a -> k -> Map k a
addEneryAtVertex = flip . adjust

increaseNeighbors :: Map Vertex Octopus -> [Vertex] -> Map Vertex Octopus
increaseNeighbors = foldl (addEneryAtVertex updateEnergy)

step :: State Grid Int
step = do
  modify $ fmap updateEnergy -- increase
  n <- flashThemBoys -- flash & recurse
  modify $ fmap reset -- reset
  pure n

flashThemBoys :: State Grid Int
flashThemBoys = do
  flashed <- gets $ filter (excessEnergy 9)
  let flashes = length flashed
  if flashes == 0
    then pure 0
    else do
      let toFlashed = fmap (const Flashed) flashed
      modify $ union toFlashed
      let n = concatMap neighbors (keys flashed)
      modify $ \g -> increaseNeighbors g n
      recursedFlashes <- flashThemBoys
      pure (recursedFlashes + flashes)

part1 :: Grid -> Int
part1 = sum . evalState (replicateM 100 step)

part2 :: Grid -> Int
part2 = (+ 1) . fromJust . elemIndex 100 . evalState (replicateM 1000 step)

parse :: [String] -> Grid
parse file = os
  where
    columns = length $ head file
    rows = length file
    vs = [Vertex x y | x <- [0 .. rows -1], y <- [0 .. columns -1]]
    os = fromList $ map (getOctopus file) vs

day11 :: Solution Grid Int Int
day11 = Solution 11 parse part1 part2

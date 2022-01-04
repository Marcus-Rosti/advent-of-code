module Day.Day15 (day15, expand) where

import AOC (Solution (Solution))
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Graph.DijkstraSimple
  ( EdgeTo (EdgeTo),
    Graph (Graph),
    Path (pathWeight),
    findPath,
  )
import Graph.DijkstraSimple.Weighters (cumulativeWeighter)

expand :: Integral a => M.Map (a, a) a -> a -> M.Map (a, a) a
expand initial to = M.fromList vs2
  where
    elements = M.toList initial
    (mx, my) = maximum $ map fst elements
    vs2 = do
      ((x, y), r) <- elements
      i <- [0 .. (to - 1)]
      j <- [0 .. (to - 1)]
      return ((x + i * (mx + 1), y + j * (my + 1)), mod (r - 1 + i + j) 9 + 1)

neighbors :: (Ord a, Num a, Ord b, Num b) => M.Map (a, a) b -> ((a, a), b) -> ((a, a), [EdgeTo (a, a) b])
neighbors vs (p@(x, y), _) = (p, edges)
  where
    toLookup = mapMaybe (\potentialNeighbor -> fmap (\riskAtNeighbor -> (potentialNeighbor, riskAtNeighbor)) (M.lookup potentialNeighbor vs)) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    edges = map (uncurry EdgeTo) toLookup

getGraph :: (Ord b, Num b, Ord e, Num e) => [((b, b), e)] -> Graph (b, b) e
getGraph = Graph . M.fromList . (map =<< neighbors . M.fromList)

-- parse
parse :: [[Char]] -> Graph (Int, Int) Int
parse input = getGraph (M.toList (expand (M.fromList t) 5))
  where
    p = map (map (read . (: []))) input
    t = [((x, y), p !! y !! x) | y <- [0 .. length p -1], x <- [0 .. length (head p) -1]]

shortestPath :: (Ord a, Num a, Ord b) => Graph b a -> b -> b -> a
shortestPath g start end = pathWeight $ fromJust $ findPath g start cumulativeWeighter end

part1 :: (Ord a, Num a, Integral b) => Graph (a, a) b -> b
part1 g = shortestPath g (0, 0) (99, 99)

part2 :: (Ord a, Num a, Integral b) => Graph (a, a) b -> b
part2 g = shortestPath g (0, 0) (499, 499)

-- day15 :: Solution Graph Risk Risk
-- day15 :: Solution [((Int, Int), Int)] Int Int
day15 = Solution 15 parse part1 part2

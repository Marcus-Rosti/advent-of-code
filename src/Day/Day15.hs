module Day.Day15 (day15) where

import AOC (Solution (Solution))
import Data.Bifunctor (second)
import Data.List (delete, find, minimumBy, sortOn)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S

type Point = (Int, Int)

type Risk = Int

type Vertex = (Point, Risk)

type AdjacentPoints = M.Map Point [Point]

type RiskAtPoint = M.Map Point Risk

type Graph = M.Map Vertex [Vertex]

data QueueElem = QueueElem Point Risk deriving (Show)

instance (Eq QueueElem) where
  (==) (QueueElem p1 _) (QueueElem p2 _) = p1 == p2
  (/=) (QueueElem p1 _) (QueueElem p2 _) = p1 /= p2

instance (Ord QueueElem) where
  compare (QueueElem _ r1) (QueueElem _ r2) = compare r1 r2

infinity :: Num a => a
infinity = 1000000

lookupWithDefault :: (Num a, Ord k) => k -> M.Map k a -> a
lookupWithDefault k m = fromMaybe infinity (M.lookup k m)

-- type Graph = [Vertex]
-- Parsing
neighbors :: [Vertex] -> Vertex -> (Vertex, [Vertex])
neighbors vs v@((x, y), _) = (v, toLookup)
  where
    toLookup = mapMaybe (\t -> fmap (\c -> (t, c)) (lookup t vs)) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

parse :: [String] -> Graph
parse input = aj
  where
    p = map (map (read . (: []))) input
    t = [((x, y), p !! y !! x) | y <- [0 .. length p -1], x <- [0 .. length (head p) -1]]
    aj = M.fromList (map (neighbors t) t)

-- minimumValueFromMap :: (Ord b) => [(a, b)] -> Maybe a
-- minimumValueFromMap [] = Nothing
-- minimumValueFromMap xs = Just (fst (minimumBy (\(_, a) (_, b) -> compare a b) xs))

-- minimumRisk :: (Ord a, Ord b) => M.Map a b -> [a] -> Maybe a
-- minimumRisk riskMap toVisit = minPoint
--   where
--     lookedup = mapMaybe (\k -> fmap (\v -> (k, v)) (M.lookup k riskMap)) toVisit
--     minPoint = minimumValueFromMap lookedup

getVertexFromGraph :: Eq a1 => M.Map (a1, b) a2 -> a1 -> Maybe (a1, b)
getVertexFromGraph g p = find (\(p2, _) -> p == p2) $ M.keys g

findNeighborsByPoint :: (Ord a1, Ord b) => M.Map (a1, b) [a] -> a1 -> [a]
findNeighborsByPoint g p = fromMaybe [] $ do
  verty <- getVertexFromGraph g p
  M.lookup verty g

--Djikstra's
djikstra :: AdjacentPoints -> RiskAtPoint -> Point -> M.Map Point Risk
djikstra adjacents risksAtPoint start = search totalRisk q
  where
    initialPoints = (map fst . M.keys) g
    q :: Q.MinQueue QueueElem
    q = Q.insert (QueueElem start 0) $ Q.fromList (map (QueueElem infinity) initialPoints)
    totalRisk = M.singleton start 0
    search :: M.Map Point Risk -> Q.MinQueue (Risk, Point) -> M.Map Point Risk
    search risks toVisit = if Q.null toVisit then risks else search (M.union (M.fromList x) risks) queue
      where
        ((riskAtNext, next), queue) = Q.deleteFindMin toVisit
        nextPoints = fromMaybe [] $ M.lookup next adjacents
        t = do
          v <- nextPoints
          let alt = maybe infinity (riskAtNext +) (M.lookup v risksAtPoint)
          let dv = lookupWithDefault v risks
          [(v, dv) | alt < dv]

-- x = filter (\(p, v) -> v < fromMaybe infinity (M.lookup p risks)) $ map (second (riskAtNext +)) nextPoints

part1 :: Graph -> Risk
part1 g = fromMaybe (-1) $ M.lookup (9, 9) (djikstra g (0, 0))

part2 :: Graph -> Risk
part2 _ = 1

day15 :: Solution Graph Risk Risk
day15 = Solution 15 parse part1 part2

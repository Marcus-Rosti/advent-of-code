module Day.Day12 (day12) where

import AOC
import Data.Char (isUpper)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

data Cave = Start | LittleBoi String | BigBoi String | End deriving (Eq, Ord)

type Graph a = M.Map a [a]

type Seen = M.Map Cave Int

type Path = [Cave]

upper :: String -> Bool
upper = all isUpper

makeCave :: String -> Cave
makeCave "start" = Start
makeCave "end" = End
makeCave str = if upper str then BigBoi str else LittleBoi str

toTupes :: [b] -> Maybe (b, b)
toTupes [a, b] = Just (a, b)
toTupes _ = Nothing

invert :: (b, a) -> (a, b)
invert (a, b) = (b, a)

toMap :: [(Cave, Cave)] -> Graph Cave
toMap = M.fromListWith (++) . map (\(a, b) -> (a, [b]))

parser :: [String] -> Graph Cave
parser linez = edges
  where
    caves = mapMaybe ((toTupes . map makeCave) . splitOn "-") linez
    edges = toMap (caves ++ map invert caves)

type Visitable = Seen -> Cave -> Bool

canVisit :: Visitable -> Visitable
canVisit _ _ (BigBoi _) = True
canVisit _ _ Start = False
canVisit _ _ End = False
canVisit f visited small = f visited small

appendToSeen :: Seen -> Cave -> Seen
appendToSeen s c = M.insertWith (+) c 1 s

walkPath :: Visitable -> Graph Cave -> Seen -> Cave -> Cave -> Path -> [Path]
walkPath visitable graph visited start end soFar = if end `elem` reached then subPaths ++ [soFar ++ [end]] else subPaths
  where
    visited' = appendToSeen visited start
    reached = M.findWithDefault [] start graph
    possibleToVisit = filter (visitable visited') reached
    nextPaths next = walkPath visitable graph visited' next end (soFar ++ [next])
    subPaths = concatMap nextPaths possibleToVisit

visitOnceTwice :: Visitable
visitOnceTwice visited next = M.notMember next visited || M.null (M.filterWithKey f visited)
  where
    f (LittleBoi _) v = v > 1
    f _ _ = False

part1 :: Graph Cave -> Int
part1 g = length $ walkPath (canVisit (flip M.notMember)) g M.empty Start End []

part2 :: Graph Cave -> Int
part2 g = length $ walkPath (canVisit visitOnceTwice) g M.empty Start End []

day12 :: Solution (Graph Cave) Int Int
day12 = Solution 12 parser part1 part2

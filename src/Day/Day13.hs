module Day.Day13 (day13) where

import AOC (Solution (Solution))
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Data.Set as S (Set, filter, fromList, map)

type Vertex = (Int, Int)

data Fold = X Int | Y Int deriving (Show)

data Parsed = Parsed Image [Fold] deriving (Show)

type Image = Set Vertex

-- instance Show Image

parsePoint :: [String] -> Maybe Vertex
parsePoint [x, y] = Just (read x, read y)
parsePoint _ = Nothing

parseFoldLine :: [String] -> Maybe Fold
parseFoldLine ["fold", "along", f] = parseFoldCommand (splitOn "=" f)
parseFoldLine _ = Nothing

parseFoldCommand :: [String] -> Maybe Fold
parseFoldCommand ["x", x] = Just (X (read x))
parseFoldCommand ["y", y] = Just (Y (read y))
parseFoldCommand _ = Nothing

parser :: [String] -> [Image]
parser linez = allFolds $ Parsed (fromList $ mapMaybe (parsePoint . splitOn ",") points) (mapMaybe (parseFoldLine . words) folds)
  where
    [points, folds] = splitOn [""] linez

visiblePoints :: Set a -> Int
visiblePoints = length

diff :: (Ord p, Num p) => p -> p -> p
diff x xf = if x >= xf then x - (2 * (x - xf)) else x

foldVertex :: Vertex -> Fold -> Vertex
foldVertex (x, y) (X xf) = (diff x xf, y)
foldVertex (x, y) (Y yf) = (x, diff y yf)

foldPaper :: Image -> Fold -> Image
foldPaper i f = filterFold f $ S.map (`foldVertex` f) i

filterFold :: Fold -> Image -> Image
filterFold (X x) = S.filter (\(xp, _) -> x /= xp)
filterFold (Y y) = S.filter (\(_, yp) -> y /= yp)

allFolds :: Parsed -> [Image]
allFolds (Parsed i fs) = tail $ scanl foldPaper i fs

part1 :: [Image] -> Int
part1 = visiblePoints . head

part2 :: [Image] -> Image
part2 = last

day13 :: Solution [Image] Int Image
day13 = Solution 13 parser part1 part2
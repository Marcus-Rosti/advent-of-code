module Day.Two.DayTwo (day2) where

import Data.Maybe
import Day.FileIO (reader)

data Position = Position Int Int | Aim Int Int Int

positionMult :: Position -> Int
positionMult (Position x y) = x * y
positionMult (Aim x y _) = x * y

data Move = Forward Int | Down Int | Up Int

initialPosition :: Position
initialPosition = Position 0 0

initialAim :: Position
initialAim = Aim 0 0 0

readMove :: [String] -> Maybe Move
readMove ["forward", x] = Just (Forward (read x))
readMove ["up", x] = Just (Up (read x))
readMove ["down", x] = Just (Down (read x))
readMove _ = Nothing

fileToMoves :: [String] -> [Move]
fileToMoves = mapMaybe (readMove . words)

navigate :: Position -> Move -> Position
navigate (Position x y) (Forward a) = Position (x + a) y
navigate (Position x y) (Up a) = Position x (y - a)
navigate (Position x y) (Down a) = Position x (y + a)
navigate (Aim x y z) (Forward a) = Aim (x + a) (a * z + y) z
navigate (Aim x y z) (Up a) = Aim x y (z - a)
navigate (Aim x y z) (Down a) = Aim x y (z + a)

navigateMoves :: [Move] -> Position
navigateMoves = foldl navigate initialPosition

aimMoves :: [Move] -> Position
aimMoves = foldl navigate initialAim

part1 :: [Move] -> Int
part1 = positionMult . navigateMoves

part2 :: [Move] -> Int
part2 = positionMult . aimMoves

day2 :: String -> IO ()
day2 file = do
  contents <- reader file
  let moves = fileToMoves contents
  print "Day 2"
  print $ part1 moves
  print $ part2 moves

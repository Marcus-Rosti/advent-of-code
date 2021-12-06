module Day.Four.DayFour (day4) where

import Data.List (find, maximumBy, minimumBy, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Day.FileIO (reader)

newtype Board = Board [[Int]] deriving (Show)

type Header = [Int]

data State = State [Int] [Int] [[Int]]

data WinState = WinState Int Int deriving (Show)

day4 :: String -> IO ()
day4 file = do
  contents <- reader file
  let (header, boards) = readBoards contents
  let wins = mapMaybe (findFinal header) boards
  print "Day 4"
  print $ findMin wins
  print $ findMax wins

readBoard :: [String] -> [Board]
readBoard [] = []
readBoard (_ : a : b : c : d : e : xs) = Board (map (map read . words) [a, b, c, d, e]) : readBoard xs
readBoard _ = []

readBoards :: [String] -> (Header, [Board])
readBoards linez = (header, boards)
  where
    header = map read $ splitOn "," $ head linez
    boards = readBoard (tail linez)

explode :: Board -> [[Int]]
explode (Board a) = a ++ transpose a

winner :: State -> Bool
winner (State _ _ board) = any null board

filterBoard :: Int -> [[Int]] -> [[Int]]
filterBoard move = map (filter (move /=))

nextState :: State -> State
nextState s@(State _ _ []) = s
nextState s@(State [] _ _) = s
nextState (State (next : unseen) seen board) = State unseen (next : seen) (filterBoard next board)

allStates :: State -> [State]
allStates s = s : allStates (nextState s)

filterFinalBoard :: [Int] -> [[Int]] -> [[Int]]
filterFinalBoard h xs = foldr filterBoard xs h

winState :: State -> Board -> WinState
winState (State unseen seen _) (Board xs) = WinState unseenCount (lastValue * boardSum)
  where
    unseenCount = length unseen
    lastValue = head seen
    boardSum = sum . map sum $ filterFinalBoard seen xs

findFinal :: Header -> Board -> Maybe WinState
findFinal header board = maybeWin
  where
    allPossible = explode board
    initialState = State header [] allPossible
    states = allStates initialState
    maybeWin = do
      win <- find winner states
      return $ winState win board

findMin :: [WinState] -> WinState
findMin = maximumBy (\(WinState a _) (WinState b _) -> compare a b)

findMax :: [WinState] -> WinState
findMax = minimumBy (\(WinState a _) (WinState b _) -> compare a b)

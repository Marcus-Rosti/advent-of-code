module Day.Base (Answer (Day), Solution) where

data Answer a b = Day Int a b deriving (Eq)

instance (Show a, Show b) => Show (Answer a b) where
  show (Day i a b) = "Day " ++ show i ++ "\n\tPart 1: " ++ show a ++ "\n\tPart 2: " ++ show b

instance (Ord a, Ord b) => Ord (Answer a b) where
  compare (Day a _ _) (Day b _ _) = compare a b

type Solution a b = String -> IO (Answer a b)

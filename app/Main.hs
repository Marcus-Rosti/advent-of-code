module Main where

import Day.Base (Answer (Day), Solution)
import Day.Five.DayFive
import Day.Four.DayFour
import Day.One.DayOne
import Day.Seven.DaySeven
import Day.Six.DaySix
import Day.Three.DayThree
import Day.Two.DayTwo
import Text.Read (Lexeme (String))

myShow :: (Show a, Show b) => IO (Answer a b) -> IO String
myShow = fmap show

solutions :: [IO String]
solutions =
  [ myShow (day1 "data/day1/input"),
    myShow (day2 "data/day2/input"),
    myShow (day3 "data/day3/input"),
    myShow (day4 "data/day4/input"),
    myShow (day5 "data/day5/input"),
    myShow (day6 "data/day6/input"),
    myShow (day7 "data/day7/input")
  ]

main :: IO ()
main = do
  sols <- sequence_ solutions
  print sols

module Main where

import Day.Five.DayFive (day5)
import Day.Four.DayFour
import Day.One.DayOne
import Day.Three.DayThree
import Day.Two.DayTwo

main :: IO ()
main = do
  day1 "data/day1/input"
  day2 "data/day2/input"
  day3 "data/day3/input"
  day4 "data/day4/input"
  day5 "data/day5/input"

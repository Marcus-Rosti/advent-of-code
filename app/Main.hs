module Main where

import AOC (debug, exec)
import Day.Day10
import Day.Day11
import Day.Day12 (day12)
import Day.Day13 (day13)
import Day.Day14 (day14)
import Day.Five.DayFive
import Day.Four.DayFour
import Day.One.DayOne
import Day.Seven.DaySeven
import Day.Six.DaySix
import Day.Three.DayThree
import Day.Two.DayTwo

main :: IO ()
main = do
  day1 "data/day1/input"
  day2 "data/day2/input"
  day3 "data/day3/input"
  day4 "data/day4/input"
  day5 "data/day5/input"
  day6 "data/day6/input"
  day7 "data/day7/input"
  exec "data/day10/input" day10
  exec "data/day11/input" day11
  exec "data/day12/input" day12
  exec "data/day13/input" day13
  exec "data/day14/input" day14

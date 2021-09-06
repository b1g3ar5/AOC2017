module Lib
    ( someFunc
    ) where


import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day14a
import Day15
import Day16
import Day17
import Day18
import Day19
import Day20
import Day21
import Day22
import Day23
import Day24
import Day25

import Data.Type.Equality (TestEquality(testEquality))
import System.TimeIt

someFunc :: IO ()
someFunc = do
    timeIt day1
    timeIt day2
    timeIt day3
    timeIt day4
    timeIt day5
    timeIt day6 
    timeIt day7 
    timeIt day8 
    timeIt day9 
    timeIt day10
    timeIt day11
    timeIt day12
    timeIt day13
    timeIt day14 -- 11s
    timeIt day15 -- 15s 
    timeIt day16 
    timeIt day17
    timeIt day18
    timeIt day19
    timeIt day20
    timeIt day21
    timeIt day22 -- 20s
    timeIt day23
    timeIt day24 
    timeIt day25
    

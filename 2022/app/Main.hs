module Main where

import Day01 (day1)
import Day02 (day2)
import Day03 (day3)
import Day04 (day4)
import Day05 (day5)
import Day06 (day6)
import Day07 (day7)
import Day08 (day8)
import Day09 (day9)
import Day10 (day10)
import Day11 (day11)
import Data.List (intersperse)

import System.Environment (getArgs)

day0 = putStrLn "🎄 Advent of Code 2022 🎄"

days = [day0, day1, day2, day3, day4, day5, day6, day7, day8, day9, day10, day11]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> sequence_ $ intersperse (putStrLn "") days
        ks -> sequence_ $ intersperse (putStrLn "") (map ((days!!) . read) ks)

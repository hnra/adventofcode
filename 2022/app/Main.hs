module Main where

import Day01 (day1)
import Day02 (day2)
import Day03 (day3)
import Day04 (day4)
import Day05 (day5)
import Data.List (intersperse)

main :: IO ()
main = do
    let
        days = [day1, day2, day3, day4, day5]
    sequence_ $ intersperse (putStrLn "") days

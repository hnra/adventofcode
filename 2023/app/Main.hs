module Main where

import Day01 (day1)
import Day02 (day2)
import Data.List (intersperse)

import System.Environment (getArgs)

day0 = putStrLn "🎄 Advent of Code 2023 🎄"

days = [ day0, day1, day2 ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> sequence_ $ intersperse (putStrLn "") days
        ks -> sequence_ $ intersperse (putStrLn "") (map ((days!!) . read) ks)

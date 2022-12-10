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
import Data.List (intersperse)

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map, (!))
import System.Environment (getArgs)

days :: Map String (IO ())
days = M.fromList
    [ ("1", day1)
    , ("2", day2)
    , ("3", day3)
    , ("4", day4)
    , ("5", day5)
    , ("6", day6)
    , ("7", day7)
    , ("8", day8)
    , ("9", day9)
    , ("10", day10)
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> sequence_ $ intersperse (putStrLn "") (M.elems days)
        ks -> sequence_ $ intersperse (putStrLn "") (map (days!) ks)

module Day1 (day1) where

import Utilities
import qualified Data.Text as T
import Data.List (sort)

type Input = [[Int]]

readInput :: IO Input
readInput = do
    input <- T.splitOn "\n\n" <$> getInput "01"
    return $ map (map tread . T.lines) input

p1 :: Input -> Int
p1 = maximum . map sum

p2 :: Input -> Int
p2 = sum . take 3 . (reverse . sort) . map sum

day1 :: IO ()
day1 = do
    input <- readInput
    putStrLn "⭐⭐ Day 1 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (p1 input)
    putStrLn $ "Part 2: " ++ show (p2 input)

module Main where

import Text.Read
import Text.Printf
import Data.Maybe

input :: IO [Integer]
input = do
    str <- readFile "inputs/day1.txt"
    let parsed = fmap readMaybe (lines str)
    return $ catMaybes parsed

p1 :: [Integer] -> Integer
p1 (x:xs@(y:_))
    | y > x = 1 + p1 xs
    | otherwise = p1 xs
p1 _ = 0

p2 :: [Integer] -> Integer
p2 (a:xs@(_:_:d:_))
    | d > a = 1 + p2 xs
    | otherwise = p2 xs
p2 _ = 0

main = do
    print "Day 1"
    ints <- input
    let a1 = p1 ints
    let a2 = p2 ints
    printf "Problem 1: %i\n" a1
    printf "Problem 2: %i\n" a2


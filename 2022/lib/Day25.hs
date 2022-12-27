module Day25 where

import Utilities (unreachable, getLinesS)
import Data.Foldable (minimumBy)

toMult :: Char -> Integer
toMult '-' = -1
toMult '=' = -2
toMult c = read (c:"")

toDec :: String -> Integer
toDec s = sum (zipWith (\c i -> toMult c * (5 ^ i)) (reverse s) [0..])

toSnafu :: Integer -> String
toSnafu = reverse . go
    where
        go 0 = []
        go n = case (n + 2) `divMod` 5 of
            (x, 0) -> '=' : go x
            (x, 1) -> '-' : go x
            (x, c) -> (head . show) (c - 2) : go x

day25 = do
    input <- getLinesS "25"
    let d = sum (map toDec input)
        part1 = toSnafu d
    putStrLn "⭐ Day 25 ⭐"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn "Part 2: Start the blender!"

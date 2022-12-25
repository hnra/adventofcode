module Day25 where

import Utilities (unreachable, getLinesS)
import Data.Foldable (minimumBy)

toMult '2' = 2
toMult '1' = 1
toMult '0' = 0
toMult '-' = -1
toMult '=' = -2
toMult _ = unreachable

toDec :: String -> Integer
toDec s = sum (zipWith (\c i -> toMult c * (5 ^ i)) (reverse s) [0..])

toSnafu d
    | one >= d = '1' : go (i - 1) (one - d)
    | largest - one >= d = '1' : go (i - 1) (one - d)
    | two >= d = '2' : go (i - 1) (two - d)
    | largest >= d = '2' : go (i - 1) (two - d)
    | otherwise = '1' : go i (5^(i+1) - d)
    where
        pow = fromIntegral . floor . logBase 5 . fromIntegral
        i = pow d
        largest = sum (map ((2*) .(5^)) [i,i-1..0])
        one = 5^i
        two = 2 * one
        go (-1) _ = []
        go i delta = c : go (i-1) (delta + d)
            where
                i5 = 5^i
                candidates = [((-2) * i5, '='), (-i5, '-'), (0, '0'), (i5, '1'), (2*i5, '2')]
                (d, c) = minimumBy (\(d1,_) (d2,_) -> abs (delta + d1) `compare` abs (delta + d2)) candidates

day25 = do
    input <- getLinesS "25"
    let d = sum (map toDec input)
        part1 = toSnafu d
    putStrLn "⭐ Day 25 ⭐"
    putStrLn $ "Part 1: " ++ show part1

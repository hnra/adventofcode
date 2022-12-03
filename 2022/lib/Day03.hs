module Day03 (day3) where

import Data.Char (isLower)

type Rucksack2 = (String, String)

day3input :: IO [String]
day3input = lines <$> readFile "inputs/day03"

part1input :: [String] -> [Rucksack2]
part1input = map (\l -> splitAt (length l `div` 2) l)

priority :: Char -> Int
priority c
    | isLower c = fromEnum c - 96
    | otherwise = fromEnum c - 38

commonChar :: (String, String) -> Char
commonChar (ss, ss') = head [s | s <- ss, s' <- ss', s == s']

p1 :: [Rucksack2] -> Int
p1 = sum . map (priority . commonChar)

type Rucksack3 = (String, String, String)

part2input :: [String] -> [Rucksack3]
part2input = groupBy3
    where
        groupBy3 (a:b:c:rest) = (a, b, c) : groupBy3 rest
        groupBy3 _ = []

common3 :: Rucksack3 -> Char
common3 (xs, ys, zs) = head [x | x <- xs, y <- ys, x == y, z <- zs, y == z]

p2 :: [Rucksack3] -> Int
p2 = sum . map (priority . common3)

day3 :: IO ()
day3 = do
    input <- day3input
    putStrLn "⭐⭐ Day 3 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (p1 $ part1input input)
    putStrLn $ "Part 2: " ++ show (p2 $ part2input input)

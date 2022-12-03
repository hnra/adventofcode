module Day03 (day3) where

import Data.Char (isLower)

type Input = [(String, String)]

day3input :: IO Input
day3input = do
    ls <- lines <$> readFile "inputs/day03"
    return $ map (\l -> splitAt (length l `div` 2) l) ls

priority :: Char -> Int
priority c
    | isLower c = fromEnum c - 96
    | otherwise = fromEnum c - 38

pairs :: [a] -> [a] -> [(a,a)]
pairs as as' = [(a, a') | a <- as, a' <- as']

commonChar :: String -> String -> Char
commonChar ss ss' = (fst . head . filter (uncurry (==))) (pairs ss ss')

p1 :: Input -> Int
p1 = sum . map (priority . uncurry commonChar)

day3 :: IO ()
day3 = do
    input <- day3input
    putStrLn "⭐ Day 3 ⭐"
    putStrLn $ "Part 1: " ++ show (p1 input)

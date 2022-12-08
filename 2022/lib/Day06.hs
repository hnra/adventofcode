module Day06 (day6) where

import Utilities (getInput)
import Data.List (nub)
import qualified Data.Text as T

p :: Int -> String -> Int
p cnt input = go cnt input
    where
        go i input@(_:rest)
            | (length . nub . take cnt) input == cnt = i
            | otherwise = go (i + 1) rest

day6 = do
    input <- T.unpack <$> getInput "06"
    putStrLn "⭐⭐ Day 6 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (p 4 input)
    putStrLn $ "Part 2: " ++ show (p 14 input)

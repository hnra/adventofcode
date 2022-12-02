module Day02 (day2) where

import Utilities
import Data.Text (Text)
import qualified Data.Text as T

data Move = Rock | Paper | Scissors
    deriving (Eq, Show)

fromStr :: Text -> Move
fromStr "X" = Rock
fromStr "A" = Rock
fromStr "Y" = Paper
fromStr "B" = Paper
fromStr "Z" = Scissors
fromStr "C" = Scissors
fromStr _ = error "Invalid input"

type Strategy = (Move, Move)

moveScore :: Move -> Int
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissors = 3

step :: Strategy -> Int
step (Rock, Paper) = 6 + moveScore Paper
step (Rock, Scissors) = moveScore Scissors
step (Paper, Rock) = moveScore Rock
step (Paper, Scissors) = 6 + moveScore Scissors
step (Scissors, Rock) = 6 + moveScore Rock
step (Scissors, Paper) = moveScore Paper
step (x, y)
    | x == y = 3 + moveScore y
    | otherwise = error "Unhandled input"

parseLine :: Text -> Strategy
parseLine l = (fromStr theirs, fromStr mine)
    where
        theirs:mine:_ = T.words l

strategies :: IO [Strategy]
strategies = do
    input <- getInput "02"
    return $ map parseLine (T.lines input) 

d1 :: [Strategy] -> Int
d1 = foldr (\s score -> score + step s) 0

day2 :: IO ()
day2 = do
    input <- strategies
    putStrLn "⭐⭐ Day 2 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (d1 input)

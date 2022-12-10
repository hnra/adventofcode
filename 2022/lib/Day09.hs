module Day09 (day9) where

import Utilities (getLines, tread)
import qualified Data.Text as T
import Data.Set (Set, insert, singleton)
import Control.Monad.Trans.State.Strict (State, get, put, execState)

data Direction = R | U | L | D deriving (Read)
type Input = (Direction, Int)

type Position = (Int, Int)
data Rope = Rope [Position] (Set Position)

day9input :: IO [Input]
day9input = map parseLine <$> getLines "09"
    where
        parseLine l = let (dir:amount:_) = T.splitOn " " l
                      in (tread dir, tread amount)

move :: Direction -> Position -> Position
move R (x, y) = (x + 1, y)
move U (x, y) = (x, y + 1)
move L (x, y) = (x - 1, y)
move D (x, y) = (x, y - 1)

follow :: Position -> Position -> Position
follow (xh, yh) t@(xt, yt)
    | adjacent = t
    | otherwise = foldr (\f p -> f p) t moves
    where
        adjacent = abs (xh - xt) < 2 && abs (yh - yt) < 2
        moves = [
            if yh == yt then id else if yh > yt then move U else move D,
            if xh == xt then id else if xh > xt then move R else move L]

step :: Input -> State Rope ()
step (d, 0) = return ()
step (d, a) = do
    (Rope rs vs) <- get
    let
        h' = move d (head rs)
        t' = foldl (\acc r -> follow (head acc) r:acc) [h'] (tail rs)
    put (Rope (reverse t') (insert (head t') vs))
    step (d, a - 1)

run :: [Position] -> [Input] -> Int
run knots input = length ps
    where
        start = Rope knots (singleton (0,0))
        (Rope _ ps) = execState (traverse step input) start

day9 :: IO ()
day9 = do
    input <- day9input
    putStrLn "⭐⭐ Day 9 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (run (replicate 2 (0, 0)) input)
    putStrLn $ "Part 2: " ++ show (run (replicate 10 (0, 0)) input)

module Day05 where

import Data.Text (Text)
import qualified Data.Text as T
import Utilities (getLines, tread)
import Data.List (transpose, foldl')

-- Letters appear on chars 1, 5, 9 etc.
parseCrateLine :: Text -> [Char]
parseCrateLine t = [T.index t i | i <- is]
    where
        cnt = (T.length t + 2) `div` 4
        is = [i * 4 + 1 | i <- [0..(cnt-1)]]

parseCrates :: [Text] -> [[Char]]
parseCrates (t:ts)
    | T.any (=='[') t = parseCrateLine t : parseCrates ts
    | otherwise = parseCrates ts
parseCrates [] = []

type Stack = [Char]

buildStacks :: [[Char]] -> [Stack]
buildStacks = map (dropWhile (==' ')) . transpose

type Move = (Int, Int, Int)

parseMoves :: [Text] -> [Move]
parseMoves [] = []
parseMoves (t:ts)
    | T.any (=='m') t = (tread cnt, tread from - 1, tread to - 1) : parseMoves ts
    | otherwise = parseMoves ts
        where (_:cnt:_:from:_:to:_) = T.splitOn " " t

day05input :: IO ([Move], [Stack])
day05input = do
    input <- getLines "05"
    let
        moves = parseMoves input
        stacks = (buildStacks . parseCrates) input
    return (moves, stacks)

move :: [Stack] -> Move -> [Stack]
move stacks (c, f, t) =
    if f < t
        then l ++ (newf:take (t-f-1) ls) ++ (newt:rs)
        else r ++ (newt:take (f-t-1) rs) ++ (newf:ls)
    where
        (l, f':ls) = splitAt f stacks
        (r, t':rs) = splitAt t stacks
        newf = drop c f'
        newt = reverse (take c f') ++ t'

p1 :: [Stack] -> [Move] -> String
p1 stacks = foldMap top . foldl' move stacks
    where
        top [] = ""
        top (c:_) = c:""

day5 :: IO ()
day5 = do
    (moves, stacks) <- day05input
    putStrLn "⭐ Day 5 ⭐"
    putStrLn $ "Part 1: " ++ show (p1 stacks moves)

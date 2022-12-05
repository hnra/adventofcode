module Day05 (day5) where

import Data.Text (Text)
import qualified Data.Text as T
import Utilities (getLines, tread)
import Data.List (transpose, foldl')
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S

-- Letters appear on chars 1, 5, 9 etc.
parseCrateLine :: Text -> [Char]
parseCrateLine t = [T.index t i | i <- is]
    where
        cnt = (T.length t + 2) `div` 4
        is = [i * 4 + 1 | i <- [0..(cnt-1)]]

parseCrateLines :: [Text] -> [[Char]]
parseCrateLines (t:ts)
    | T.any (=='[') t = parseCrateLine t : parseCrateLines ts
    | otherwise = parseCrateLines ts
parseCrateLines [] = []

type Stack = Seq Char
type Stacks = Seq Stack

buildStacks :: [[Char]] -> Stacks
buildStacks =
    S.fromList . map (S.fromList . dropWhile (==' ')) . transpose

type Move = (Int, Int, Int)

parseMoves :: [Text] -> [Move]
parseMoves [] = []
parseMoves (t:ts)
    | T.any (=='m') t =
        (tread cnt, tread from - 1, tread to - 1) : parseMoves ts
    | otherwise = parseMoves ts
    where (_:cnt:_:from:_:to:_) = T.splitOn " " t

day05input :: IO ([Move], Stacks)
day05input = do
    input <- getLines "05"
    let
        moves = parseMoves input
        stacks = (buildStacks . parseCrateLines) input
    return (moves, stacks)

data CrateMover = CM9000 | CM9001

move :: CrateMover -> Stacks -> Move -> Stacks
move m stacks (c, f, t) = ss'
    where
        order = case m of
            CM9000 -> S.reverse
            CM9001 -> id
        moved = (order . S.take c) (S.index stacks f)
        ss = S.adjust' (moved ><) t stacks
        ss' = S.adjust' (S.drop c) f ss

top :: Stack -> String
top s = case S.lookup 0 s of
    Just c -> c:""
    Nothing -> ""

p1 :: Stacks -> [Move] -> String
p1 stacks = foldMap top . foldl' (move CM9000) stacks

p2 :: Stacks -> [Move] -> String
p2 stacks = foldMap top . foldl' (move CM9001) stacks

day5 :: IO ()
day5 = do
    (moves, stacks) <- day05input
    putStrLn "⭐⭐ Day 5 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (p1 stacks moves)
    putStrLn $ "Part 2: " ++ show (p2 stacks moves)

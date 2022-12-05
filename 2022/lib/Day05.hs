module Day05 (day5) where

import Data.Text (Text)
import qualified Data.Text as T
import Utilities (getLines, tread)
import Data.List (transpose, foldl')
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S

type Stack = Seq Char
type Stacks = Seq Stack
type Move = (Int, Int, Int)

parseStacks :: [Text] -> Stacks
parseStacks = rotate . parseLines
    where
        parse = map (`T.index` 1) . T.chunksOf 4
        parseLines = map parse . filter (T.any (=='['))
        rotate = S.fromList . map (S.fromList . dropWhile (==' ')) . transpose

parseMoves :: [Text] -> [Move]
parseMoves = map (parse . T.splitOn " ") . filter (T.isPrefixOf "move")
    where
        parse (_:c:_:f:_:t:_) = (tread c, tread f - 1, tread t - 1)

day05input :: IO ([Move], Stacks)
day05input = do
    input <- getLines "05"
    let
        moves = parseMoves input
        stacks = parseStacks input
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

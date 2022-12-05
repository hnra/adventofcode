module Day05 (day5) where

import Data.Text (Text)
import qualified Data.Text as T
import Utilities (getLines, tread)
import Data.List (transpose, foldl')
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S
import Control.Monad.Trans.State.Strict (State, get, put, execState)

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
    return (parseMoves input, parseStacks input)

data CrateMover = CM9000 | CM9001
type CrateState = State Stacks ()

move :: CrateMover -> Move -> CrateState
move m (c, f, t) = do
    stacks <- get
    let
        order = case m of
            CM9000 -> S.reverse
            CM9001 -> id
        moved = (order . S.take c) (S.index stacks f)
        ss = S.adjust' (moved ><) t stacks
        ss' = S.adjust' (S.drop c) f ss
    put ss'
    return ()

run :: CrateMover -> [Move] -> Stacks -> String
run m moves = foldMap top . execState (traverse (move m) moves)
    where
        top s = case S.lookup 0 s of
            Just c -> c:""
            Nothing -> ""

day5 :: IO ()
day5 = do
    (moves, stacks) <- day05input
    putStrLn "⭐⭐ Day 5 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (run CM9000 moves stacks)
    putStrLn $ "Part 2: " ++ show (run CM9001 moves stacks)

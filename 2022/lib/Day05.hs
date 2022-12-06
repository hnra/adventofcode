module Day05 (day5) where

import Data.Text (Text)
import qualified Data.Text as T
import Utilities (getLines, tread)
import Data.List (transpose, foldl')
import GHC.Arr (readSTArray, writeSTArray, freezeSTArray, thawSTArray, STArray, elems, listArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (foldM)

type Stack = [Char]
type Stacks = [Stack]
type Move = (Int, Int, Int)

parseStacks :: [Text] -> Stacks
parseStacks = rotate . parseLines
    where
        parse = map (`T.index` 1) . T.chunksOf 4
        parseLines = map parse . filter (T.any (=='['))
        rotate = map (dropWhile (==' ')) . transpose

parseMoves :: [Text] -> [Move]
parseMoves = map (parse . T.splitOn " ") . filter (T.isPrefixOf "move")
    where
        parse (_:c:_:f:_:t:_) = (tread c, tread f - 1, tread t - 1)

day05input :: IO ([Move], Stacks)
day05input = do
    input <- getLines "05"
    return (parseMoves input, parseStacks input)

data CrateMover = CM9000 | CM9001

move :: CrateMover -> STArray s Int Stack -> Move -> ST s (STArray s Int Stack)
move m a (c, f, t) = do
    let order = case m of
            CM9000 -> reverse
            CM9001 -> id
    from <- readSTArray a f
    to <- readSTArray a t
    writeSTArray a f (drop c from)
    writeSTArray a t (order (take c from) ++ to)
    pure a

run :: CrateMover -> [Move] -> Stacks -> String
run m moves stacks = runST $ do
    arr <- thawSTArray $ listArray (0, length stacks - 1) stacks
    stacks <- elems <$> (freezeSTArray =<< foldM (move m) arr moves)
    return $ foldMap top stacks
    where
        top [] = ""
        top (s:_) = s:""

day5 :: IO ()
day5 = do
    (moves, stacks) <- day05input
    putStrLn "⭐⭐ Day 5 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (run CM9000 moves stacks)
    putStrLn $ "Part 2: " ++ show (run CM9001 moves stacks)

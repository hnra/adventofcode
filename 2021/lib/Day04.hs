module Day04 where

import Utilities (getInput, tread, unreachable)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (transpose)
import Debug.Trace
import Data.Maybe (isNothing)

type Draws = [Int]
data Board = Board { _numbers :: [[Int]], _marked :: [Int] }
    deriving Show

parseBoard :: Text -> Board
parseBoard boardStr = Board { _numbers = filter (not . null) numbers, _marked = [] }
    where
        rows = T.splitOn "\n" boardStr
        numbers = map (map tread . T.words) rows

day4input :: IO (Draws, [Board])
day4input = do
    input <- T.splitOn "\n\n" <$> getInput "04"
    let (draws, boards) = splitAt 1 input
    return ((map tread . T.splitOn "," . head) draws, map parseBoard boards)

mark :: Int -> Board -> Board
mark i b = b { _marked = i : _marked b }

hasBingo :: Board -> Maybe [Int]
hasBingo b
    | rows || cols = Just $ concatMap (filter notMarked) (_numbers b)
    | otherwise = Nothing
    where
        bingo = all (`elem` _marked b)
        notMarked = not . (`elem` _marked b)
        rows = any bingo (_numbers b)
        cols = any bingo ((transpose . _numbers) b)

hasNumber :: Int -> Board -> Bool
hasNumber i = any (elem i) . _numbers

findBingo :: [Board] -> Maybe [Int]
findBingo [] = Nothing
findBingo (b:bs) =
    case hasBingo b of
        Nothing -> findBingo bs
        is -> is

p1 :: [Board] -> [Int] -> Int
p1 bs [] = unreachable
p1 bs (i:is) =
    case findBingo bs' of
        Just is' -> i * sum is'
        Nothing -> p1 bs' is
    where
        bs' = map (\b -> if hasNumber i b then mark i b else b) bs

p2 :: [Board] -> [Int] -> Int
p2 bs [] = unreachable
p2 bs (i:is)
    | null bs'' =
        case findBingo bs' of
            Just is' -> i * sum is'
            Nothing -> p2 bs'' is
    | otherwise = p2 bs'' is
    where
        bs' = map (\b -> if hasNumber i b then mark i b else b) bs
        bs'' = filter (isNothing . hasBingo) bs'

day4 = do
    (draws, boards) <- day4input
    putStrLn "⭐⭐ Day 4 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (p1 boards draws)
    putStrLn $ "Part 2: " ++ show (p2 boards draws)

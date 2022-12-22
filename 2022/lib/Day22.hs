module Day22 where

import qualified Data.Text as T
import Utilities (getLines, hm2d, unreachable)
import Data.Char (isAlpha, isDigit)

import Data.HashMap.Strict (HashMap, (!?))
import qualified Data.HashMap.Strict as HM
import Data.List (transpose)

type Position = (Int, Int)
type Direction = (Int, Int)
type Maze = HashMap Position Char

day22input = do
    input <- getLines "22"
    let
        maze = takeWhile (not . T.any isAlpha) input
        width = maximum (map T.length maze)
        padded = map (T.justifyLeft width ' ') maze
        route = dropWhile (not . T.any isAlpha) input
    return (hm2d ((transpose . map T.unpack) padded), (T.unpack . head) route)

(x, y) °+ (x', y') = (x+x', y+y')
(x, y) °- (x', y') = (x-x', y-y')
(x, y) °* (x', y') = (x*x'-y*y', x*y'+y*x')

rotate 'L' = (°*) (0, -1)
rotate 'R' = (°*) (0, 1)
rotate _ = unreachable

dirToNum (1,0) = 0
dirToNum (0,1) = 1
dirToNum (-1,0) = 2
dirToNum (0,-1) = 3
dirToNum _ = unreachable

wrap :: Maze -> Direction -> Position -> Position
wrap maze d p
    | isEdge p = wrap' p
    | otherwise = p
    where
        wrap' p = head (filter isEdge coords) °+ d
            where coords = map (\i -> p °- ((i, 0) °* d)) [1..]
        isEdge p = case maze !? p of
            Just ' ' -> True
            Nothing -> True
            _ -> False

move :: Maze -> Direction -> Position -> Int -> Position
move _ _ p 0 = p
move maze dir p i
    | c == '#' = p
    | otherwise = move maze dir p' (i-1)
    where
        p' = wrap maze dir (p °+ dir)
        c = maze HM.! p'

run :: Maze -> String -> Direction -> Position -> (Direction, Position)
run _ [] dir pos = (dir, pos)
run maze (c:ss) dir pos = run maze ss' dir' (move maze dir' pos amount)
    where
        dir' = rotate c dir
        amount = read $ takeWhile isDigit ss
        ss' = dropWhile isDigit ss

getStart :: Maze -> Position
getStart maze = (smallestX, 0)
    where
        topRow = HM.filterWithKey (\k _ -> snd k == 0) maze
        openCols = HM.filter (=='.') topRow
        smallestX = minimum $ map fst (HM.keys openCols)

day22 = do
    (maze, route) <- day22input
    let
        pos = HM.keys
        (dir, (x, y)) = run maze ('R':route) (0,-1) (getStart maze)
        part1 = 1000 * (y + 1) + 4 * (x + 1) + dirToNum dir
    putStrLn "⭐ Day 21 ⭐"
    putStrLn $ "Part 1: " ++ show part1

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

size = 50
b = size - 1

day22input = do
    input <- getLines "22"
    let
        maze = takeWhile (not . T.any isAlpha) input
        width = maximum (map T.length maze)
        padded = map (T.justifyLeft width ' ') maze
        route = dropWhile (not . T.any isAlpha) input
    return (hm2d (map T.unpack padded), (T.unpack . head) route)

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

isEdge maze p = case maze !? p of
    Just ' ' -> True
    Nothing -> True
    _ -> False

wrap :: Maze -> Direction -> Position -> Position
wrap maze d p
    | isEdge maze p = wrap' p
    | otherwise = p
    where
        wrap' p = head (filter (isEdge maze) coords) °+ d
            where coords = map (\i -> p °- ((i, 0) °* d)) [1..]

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

inRange (x, y) ((lowX, lowY), (highX, highY)) =
    x >= lowX && x <= highX && y >= lowY && y <= highY

cubeBound :: (Int, Int) -> ((Int, Int), (Int, Int))
cubeBound (left, top) = ((left, top), (left + b, top + b))

cubeRange :: Char -> ((Int, Int), (Int, Int))
cubeRange '1' = cubeBound (size, 0)
cubeRange '2' = cubeBound (2 * size, 0)
cubeRange '3' = cubeBound (size, size)
cubeRange '4' = cubeBound (0, 2 * size)
cubeRange '5' = cubeBound (size, 2 * size)
cubeRange '6' = cubeBound (0, 3 * size)
cubeRange _ = unreachable

cube :: Position -> Char
cube xy = (head . filter (inRange xy . cubeRange)) ['1'..'6']

cubeCoord :: Char -> Position -> Position
cubeCoord c (x, y) = (x - lowX, y - lowY)
    where ((lowX, lowY), _) = cubeRange c

mazeCoord :: Char -> Position -> Position
mazeCoord c (x, y) = (x + lowX, y + lowY)
    where ((lowX, lowY), _) = cubeRange c

edge '1' (-1,  0) (x, y) = ('4', ( 1,  0), (0, b - y))
edge '1' ( 0, -1) (x, y) = ('6', ( 1,  0), (0, x    ))
edge '2' ( 0, -1) (x, y) = ('6', ( 0, -1), (x, b    ))
edge '2' ( 1,  0) (x, y) = ('5', (-1,  0), (b, b - y))
edge '2' ( 0,  1) (x, y) = ('3', (-1,  0), (b, x    ))
edge '3' (-1,  0) (x, y) = ('4', ( 0,  1), (y, 0    ))
edge '3' ( 1,  0) (x, y) = ('2', ( 0, -1), (y, b    ))
edge '4' (-1,  0) (x, y) = ('1', ( 1,  0), (0, b - y))
edge '4' ( 0, -1) (x, y) = ('3', ( 1,  0), (0, x    ))
edge '5' ( 1,  0) (x, y) = ('2', (-1,  0), (b, b - y))
edge '5' ( 0,  1) (x, y) = ('6', (-1,  0), (b, x    ))
edge '6' (-1,  0) (x, y) = ('1', ( 0,  1), (y, 0    ))
edge '6' ( 1,  0) (x, y) = ('5', ( 0, -1), (y, b    ))
edge '6' ( 0,  1) (x, y) = ('2', ( 0,  1), (x, 0    ))
edge c d xy = error ("Cannot wrap " ++ show (c, d, xy))

cubeWrap :: Direction -> Position -> (Direction, Position)
cubeWrap d p =
    (d', mazeCoord c' p'')
    where
        c = cube p
        p' = cubeCoord c p
        (c', d', p'') = edge c d p'

cubeMove :: Maze -> Direction -> Position -> Int -> (Direction, Position)
cubeMove _ d p 0 = (d, p)
cubeMove maze d p i
    | isEdge maze p' =
        case maze HM.! p'' of
            '#' -> (d, p)
            _ -> cubeMove maze d' p'' (i-1)
    | otherwise = case maze HM.! p' of
        '#' -> (d, p)
        _ -> cubeMove maze d p' (i-1)
    where
        p' = p °+ d
        (d', p'') = cubeWrap d p

runCube :: Maze -> String -> Direction -> Position -> (Direction, Position)
runCube _ [] dir pos = (dir, pos)
runCube maze (c:ss) dir pos = runCube maze ss' dir' pos'
    where
        amount = read $ takeWhile isDigit ss
        ss' = dropWhile isDigit ss
        (dir', pos') = cubeMove maze (rotate c dir) pos amount

day22 = do
    (maze, route) <- day22input
    let
        pos = HM.keys
        (dir, (x, y)) = run maze ('R':route) (0,-1) (getStart maze)
        (dir', (x', y')) = runCube maze ('R':route) (0,-1) (getStart maze)
        part1 = 1000 * (y + 1) + 4 * (x + 1) + dirToNum dir
        part2 = 1000 * (y' + 1) + 4 * (x' + 1) + dirToNum dir'
    putStrLn "⭐⭐ Day 22 ⭐⭐"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2

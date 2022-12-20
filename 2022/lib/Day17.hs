module Day17 where

import Data.Set (Set)
import qualified Data.Set as S
import Utilities (unreachable, getInputS)
import Data.Vector (Vector)
import qualified Data.Vector as V

data Coord = Coord { _x :: Int, _y :: Int } deriving (Eq, Show)

instance Ord Coord where
    (Coord x y) <= (Coord x' y')
        | y == y' = x <= x'
        | otherwise = y <= y'

type Shape = [Coord]
type Cave = Set Coord

dash = map (uncurry Coord) [(0, 0), (1, 0), (2, 0), (3, 0)]
plus = map (uncurry Coord) [(1, 2), (1, 0), (0, 1), (2, 1)]
jay = map (uncurry Coord) [(2, 2), (2, 1), (0, 0), (1, 0), (2, 0)]
pipe = map (uncurry Coord) [(0, 3), (0, 0), (0, 1), (0, 2)]
square = map (uncurry Coord) [(1, 1), (0, 0), (1, 0), (0, 1)]

shapes = [dash, plus, jay, pipe, square]

day17input :: IO String
day17input = init <$> getInputS "17"

addCoord :: Coord -> Coord -> Coord
addCoord (Coord x y) (Coord x' y') = Coord (x + x') (y + y')

hasRock :: Cave -> Coord -> Bool
hasRock cave coord@(Coord x y)
    | x < 0 || x > 6 = True
    | y < 1 = True
    | otherwise = coord `S.member` cave

move :: Cave -> Coord -> Shape -> Char -> Coord
move cave coord@(Coord x y) shape c
    | c == '<' && isFree (x-1, y) = Coord (x-1) y
    | c == '>' && isFree (x+1, y) = Coord (x+1) y
    | c == 'v' && isFree (x, y-1) = Coord x (y-1)
    | otherwise = coord
    where
        isFree (x, y) = all (\(Coord x' y') -> not (hasRock cave (Coord (x + x') (y + y')))) shape

step :: String -> Coord -> Shape -> Cave -> (Cave, String)
step (c:s) xy shape cave
    | atEnd = (cave `S.union` S.fromList shape', s)
    | otherwise = step s xy'' shape cave
    where
        xy' = move cave xy shape c
        xy'' = move cave xy' shape 'v'
        atEnd = xy' == xy''
        shape' = map (addCoord xy') shape
step _ _ _ _ = unreachable

guessLoop :: Eq a => [a] -> Maybe (Int, Int)
guessLoop caves = do
    let coords = V.fromList caves
    top <- coords V.!? 0
    let
        nextMatch i = do
            curr <- coords V.!? i
            if top == curr then return i else nextMatch (i+1)
        guess i = do
            match <- nextMatch i
            let isLoop i = case coords V.!? (match * i) of
                    Nothing -> True
                    Just future -> future == top
            if all isLoop [2,3] then return match else guess (match + 1)
    ix <- guess 1
    return (ix, 0)

maxY :: Cave -> Int
maxY cave
    | S.null cave = 0
    | otherwise = _y (head (S.toDescList cave))

minY :: Cave -> Int
minY cave
    | S.null cave = 0
    | otherwise = _y (head (S.toAscList cave))

layers :: [Cave] -> [Cave]
layers [] = []
layers [c] = [normalize c]
layers (c:c':cs) = normalize (c S.\\ c') : layers (c':cs)

normalize :: Cave -> Cave
normalize cave = S.map (addCoord (Coord 0 (-deltaY))) cave
    where deltaY = minY cave

run :: String -> [Cave]
run input = map fst steps
    where
        cave = S.empty
        streams = (concat . repeat) input
        incoming = (concat . repeat) shapes
        steps = scanl (
            \(c, ss) shape -> step ss (Coord 2 (maxY c + 4)) shape c)
            (cave, streams) incoming

findLoop :: [Cave] -> Maybe (Int, Int)
findLoop caves = guessLoop ls
    where
        len = length caves
        cs = take len (cycle [0..(length shapes-1)])
        ys = map maxY caves
        ydeltas = 0 : zipWith (-) (tail ys) ys
        ls = zip (zip (reverse ydeltas) (reverse cs)) (layers (reverse caves))

part2 :: [Cave] -> Int -> Int
part2 caves i =
    case findLoop (take i caves) of
        Just (len, loopIx) -> yNow + (yDelta * itDiv) + (yRem - yStart)
            where
                ix = i - loopIx
                yNow = maxY (caves !! ix)
                yStart = maxY (caves !! (ix - len))
                yDelta = yNow - yStart
                itLeft = 1000000000000 - ix
                itDiv = itLeft `div` len
                itRem = itLeft `rem` len
                yRem = maxY (caves !! (ix - len + itRem))
        Nothing -> part2 caves (i+1)

day17 :: IO ()
day17 = do
    input <- day17input
    let caves = run input
    putStrLn "⭐⭐ Day 17 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (maxY (caves !! 2022))
    putStrLn $ "Part 2: " ++ show (part2 caves 5000)

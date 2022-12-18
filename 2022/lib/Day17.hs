module Day17 where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Utilities (unreachable)

type Coord = (Int, Int)
type Shape = [Coord]
type Cave = HashSet Coord

dash = [(0, 0), (1, 0), (2, 0), (3, 0)]
plus = [(1, 2), (1, 0), (0, 1), (2, 1)]
jay = [(2, 2), (2, 1), (0, 0), (1, 0), (2, 0)]
pipe = [(0, 3), (0, 0), (0, 1), (0, 2)]
square = [(1, 1), (0, 0), (1, 0), (0, 1)]

shapes = [dash, plus, jay, pipe, square]

day17input :: IO String
day17input = init <$> readFile "inputs/day17"

addCoord :: Coord -> Coord -> Coord
addCoord (x, y) (x', y') = (x + x', y + y')

hasRock :: Cave -> Coord -> Bool
hasRock cave (x, y)
    | x < 0 || x > 6 = True
    | y < 1 = True
    | otherwise = (x, y) `HS.member` cave

move :: Cave -> Coord -> Shape -> Char -> Coord
move cave (x, y) shape c
    | c == '<' && isFree (x-1, y) = (x-1, y)
    | c == '>' && isFree (x+1, y) = (x+1, y)
    | c == 'v' && isFree (x, y-1) = (x, y-1)
    | otherwise = (x, y)
    where
        isFree (x, y) = all (\(x', y') -> not (hasRock cave (x + x', y + y'))) shape

step :: String -> Coord -> Shape -> Cave -> (Cave, String, Int)
step (c:s) xy shape cave
    | atEnd =
        ( cave `HS.union` HS.fromList shape'
        , s, maxY)
    | otherwise = step s xy'' shape cave
    where
        xy' = move cave xy shape c
        xy'' = move cave xy' shape 'v'
        atEnd = xy' == xy''
        shape' = map (addCoord xy') shape
        maxY = snd (head shape')
step _ _ _ _ = unreachable

run :: String -> [(Cave, Int)]
run input = map (\(c, _, maxY) -> (c, maxY)) steps
    where
        cave = HS.empty
        streams = (concat . repeat) input
        incoming = (concat . repeat) shapes
        steps = scanl (
            \(c, ss, y) shape -> let (c', ss', y') = step ss (2, y + 4) shape c in (c', ss', max y y'))
            (cave, streams, 0) incoming

day17 :: IO ()
day17 = do
    input <- day17input
    let
        caves = run input
    putStrLn "⭐ Day 17 ⭐"
    putStrLn $ "Part 1: " ++ show (snd (caves !! 2022))

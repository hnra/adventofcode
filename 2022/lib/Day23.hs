module Day23 where

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.MultiSet as MS
import Utilities (getLinesS, hm2d, unreachable, paintGrid)
import Data.List (intersperse, transpose)

type Coord = (Int, Int)
type Positions = HashSet Coord
data Direction = North | South | West | East deriving Enum

day23input :: IO Positions
day23input = do
    input <- getLinesS "23"
    let positions = HM.filter (=='#') (hm2d input)
    (return . HS.fromList . HM.keys) positions

adjacent :: Direction -> Coord -> [Coord]
adjacent North (x, y) = [(x, y - 1), (x - 1, y - 1), (x + 1, y - 1)]
adjacent South (x, y) = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
adjacent West (x, y) = [(x - 1, y), (x - 1, y - 1), (x - 1, y + 1)]
adjacent East (x, y) = [(x + 1, y), (x + 1, y - 1), (x + 1, y + 1)]

consider :: Int -> [Direction]
consider i = take 4 $ drop (i `mod` 4) (cycle (enumFrom North))

proposal :: Int -> Positions -> Coord -> Coord
proposal i pos xy
    | null adjs = xy
    | length adjs == 4 = xy
    | otherwise = head (adjacent (head adjs) xy)
    where
        dirs = consider i
        adjs = filter (\d -> not (any (`HS.member` pos) (adjacent d xy))) dirs

move :: Positions -> [(Coord, Coord)] -> Positions
move pos moves = HS.fromList newPos
    where
        props = MS.fromList $ map snd moves
        newPos = map (\(curr, p) -> if MS.occur p props > 1 then curr else p) moves

bounds :: Positions -> ((Int, Int), (Int, Int))
bounds pos = ((minimum xs, minimum ys), (maximum xs, maximum ys))
    where
        xs = map fst (HS.toList pos)
        ys = map snd (HS.toList pos)

emptyTiles :: Positions -> Int
emptyTiles pos = (1 + maxX - minX) * (1 + maxY - minY) - HS.size pos
    where ((minX, minY), (maxX, maxY)) = bounds pos

step :: Positions -> Int -> Positions
step pos i = move pos props
    where props = map (\p -> (p, proposal i pos p)) (HS.toList pos)

paint :: Positions -> IO ()
paint pos = paintGrid (bounds pos) (\xy -> if xy `HS.member` pos then '#' else '.')

firstDup :: Eq a => [a] -> Int
firstDup xs = go 0 xs
    where
        go i (x:y:xs)
            | x == y = i + 1
            | otherwise = go (i + 1) (y:xs)
        go _ _ = error "No dups"

day23 :: IO ()
day23 = do
    input <- day23input
    let runs = scanl step input [0..]
        part1 = emptyTiles (runs !! 10)
        part2 = firstDup runs
    putStrLn "⭐⭐ Day 23 ⭐⭐"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2

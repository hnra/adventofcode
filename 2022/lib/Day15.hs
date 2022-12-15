module Day15 where

import Utilities (getInput)
import Data.Attoparsec.Text

type Coord = (Int, Int)
data SB = SB Coord Coord Int deriving Show

beacon (SB _ b _) = b

pLine :: Parser SB
pLine = do
    sx <- string "Sensor at x=" *> signed decimal
    sy <- string ", y=" *> signed decimal
    bx <- string ": closest beacon is at x=" *> signed decimal
    by <- string ", y=" *> signed decimal <* endOfLine
    return $ SB (sx, sy) (bx, by) (manhattan (sx, sy) (bx, by))

day15input :: IO [SB]
day15input = do
    input <- getInput "15"
    let (Right coords) = parseOnly (many' pLine) input
    return coords

manhattan :: Coord -> Coord -> Int
manhattan (x, y) (x', y') = abs (x - x') + abs (y - y')

xRangeAtY :: Int -> SB -> Coord
xRangeAtY y (SB (x, y') _ md) = (x - width, x + width)
    where width = md - abs (y - y')

cannotBeBeacon :: Coord -> SB -> Bool
cannotBeBeacon xy (SB xy' _ md) = manhattan xy xy' <= md

removeOverlap :: Coord -> Coord -> [Coord]
removeOverlap (a, b) (c, d)
    | b < c || a > d = [(c, d)]
    | a <= c && b >= d = []
    | a <= c && b <= d = [(b+1, d)]
    | a >= c && b >= d = [(c, a-1)]
    | otherwise = [(c, a-1), (b+1, d)]

removeRange :: Coord -> [Coord] -> [Coord]
removeRange ab = concatMap (removeOverlap ab)

possibleXs :: [SB] -> Int -> [Coord]
possibleXs ss y = go [(0, 4000000)] ss
    where
        go [] _ = []
        go ranges [] = ranges
        go ranges (s:ss) = go ranges' ss
            where
                ab = xRangeAtY y s
                ranges' = removeRange ab ranges

part1 :: Int -> [SB] -> Int
part1 y input = length cannotBe
    where
        beacons = map beacon input
        ranges = map (xRangeAtY y) input
        a = minimum (map fst ranges)
        b = maximum (map snd ranges)
        coords = filter (not . (`elem` beacons)) (zip [a..b] (repeat y))
        cannotBe = filter (\xy -> any (cannotBeBeacon xy) input) coords

part2 :: Int -> [SB] -> Int
part2 bound input = 4000000 * x + y
    where 
        ys = [0..bound]
        (y, (x, _):_) = 
            (head . filter (not . null . snd) . zip ys . map (possibleXs input)) ys

day15 :: IO ()
day15 = do
    input <- day15input
    putStrLn "⭐⭐ Day 15 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (part1 2000000 input)
    putStrLn $ "Part 2: " ++ show (part2 4000000 input)

module Day14 where

import qualified Data.Text as T
import Utilities (getLines, tread, unreachable)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Maybe (isNothing)

type Path = [(Int, Int)]
type Cave = HashMap (Int, Int) Char

rock = '#'

day14input :: IO [Path]
day14input = do
    lines <- map (T.splitOn "->") <$> getLines "14"
    let paths = (map . map) (parse . T.splitOn ",") lines
    return paths
    where
        parse (t1:t2:_) = (tread t1, tread t2)
        parse _ = unreachable

getBounds :: [Path] -> (Int, Int)
getBounds paths = (maximum xs, maximum ys)
    where
        (xs, ys) = (unzip . concat) paths

run :: (Int, Int) -> Bool -> Int -> Maybe Cave -> Maybe Cave
run _ _ _ Nothing = Nothing
run (x, y) floor ybound (Just cave)
    | floor && y == ybound = Just (HM.insert (x, y) rock cave)
    | not floor && y >= ybound = Nothing
    | otherwise = case spots of
        (xy:_) -> run xy floor ybound (Just cave)
        [] -> if floor && y == 0 
                then Nothing
                else Just (HM.insert (x, y) rock cave)
    where
        coords = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
        spots = filter (not . (`HM.member` cave)) coords

toCave :: Path -> Cave
toCave ((x,y):xy'@(x',y'):rest) = 
    HM.fromList (zip [(x'', y'')
                     | y'' <- [(min y y')..(max y y')]
                     , x'' <- [(min x x')..(max x x')]
                     ] (repeat rock)) 
    `HM.union` toCave (xy':rest)
toCave _ = HM.empty

day14 = do
    input <- day14input
    let
        (_, maxY) = getBounds input
        cave = HM.unions (map toCave input)
        rocks = length . filter (==rock) . HM.elems
        startRocks = rocks cave
        go floor ybound = until (isNothing . runner) runner (Just cave)
            where runner = run (500, 0) floor ybound
        (Just part1) = rocks <$> go False maxY
        (Just part2) = rocks <$> go True (maxY+1)
    putStrLn "⭐⭐ Day 14 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (part1 - startRocks)
    putStrLn $ "Part 2: " ++ show (part2 - startRocks)

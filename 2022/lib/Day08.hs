module Day08 (day8) where

import GHC.Arr (Array, array, numElements, (!), bounds)

type Forest = Array (Int, Int) Int
type Coord = (Int, Int)
type Bounds = (Int, Int)

day8input :: IO Forest
day8input = do
    lines <- lines <$> readFile "inputs/day08"
    let
        forestList = (map . map) (read . (:"")) lines
        width = length (head forestList)
        height = length forestList
        xs = concatMap (zip [0..]) forestList
        ycoords = concatMap (replicate height) [0..]
        xys = zipWith (\y (x, t) -> ((x, y), t)) ycoords xs
    return $ array ((0, 0), (width - 1, height - 1)) xys

lineOfSight :: Bounds -> Coord -> ([Coord], [Coord], [Coord], [Coord])
lineOfSight (width, height) (x, y) = (left, right, top, bottom)
    where
        left = [(x', y) | x' <- reverse [0..x-1]]
        right = [(x', y) | x' <- [x+1..width-1]]
        top = [(x, y') | y' <- reverse [0..y-1]]
        bottom = [(x, y') | y' <- [y+1..height-1]]

isVisible :: Forest -> Coord -> Bool
isVisible arr (x, y) = any (all isSmaller) [left, right, top, bottom]
    where
        tree = arr ! (x, y)
        isSmaller = (<tree) . (arr!)
        ((_, _), (width, height)) = bounds arr
        (left, right, top, bottom) = lineOfSight (width, height) (x, y)

takeWhileLeq :: Ord a => a -> [a] -> [a]
takeWhileLeq _ [] = []
takeWhileLeq y (x:xs)
    | x < y = x : takeWhileLeq y xs
    | otherwise = [x]

scenicScore :: Forest -> Coord -> Int
scenicScore arr (x, y) =
    product (map (length . visibleTrees) [left, right, top, bottom])
    where
        tree = arr ! (x, y)
        ((_, _), (width, height)) = bounds arr
        (left, right, top, bottom) = lineOfSight (width, height) (x, y)
        visibleTrees = takeWhileLeq tree . map (arr!)

p2 :: Forest -> Int
p2 arr = (maximum . map (scenicScore arr)) interior
    where
        ((_, _), (width, height)) = bounds arr
        interior = [(x, y) | x <- [1..width-2], y <- [1..height-2]]

p1 :: Forest -> Int
p1 arr = height * 2 + width * 2 - 4 + length (filter (isVisible arr) interior)
    where
        ((_, width), (_, height)) = bounds arr
        interior = [(x, y) | x <- [1..width-2], y <- [1..height-2]]

day8 :: IO ()
day8 = do
    arr <- day8input
    let
        ((_, _), (width, height)) = bounds arr
        ixs = [(x, y) | x <- [1..width-2], y <- [1..height-2]]
    putStrLn "⭐⭐ Day 8 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (p1 arr)
    putStrLn $ "Part 2: " ++ show (p2 arr)

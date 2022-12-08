module Day08 where

import GHC.Arr (Array, listArray, numElements, (!))

type Forest = Array Int (Array Int Int)
type Coord = (Int, Int)
type Bounds = (Int, Int)

day8input :: IO Forest
day8input = do
    lines <- lines <$> readFile "inputs/day08"
    let forestList = (map . map) (read . (:"")) lines
        width = length (head forestList)
        height = length forestList
    return $ listArray (0, height - 1) (map (listArray (0, width - 1)) forestList)

lineOfSight :: Bounds -> Coord -> ([Coord], [Coord], [Coord], [Coord])
lineOfSight (width, height) (x, y) = (left, right, top, bottom)
    where
        left = [(x', y) | x' <- reverse [0..x-1]]
        right = [(x', y) | x' <- [x+1..width-1]]
        top = [(x, y') | y' <- reverse [0..y-1]]
        bottom = [(x, y') | y' <- [y+1..height-1]]

isVisible :: Forest -> Coord -> Bool
isVisible arr (x, y) =
    all (\(x, y) -> (arr ! y) ! x < tree) left ||
    all (\(x, y) -> (arr ! y) ! x < tree) right ||
    all (\(x, y) -> (arr ! y) ! x < tree) top ||
    all (\(x, y) -> (arr ! y) ! x < tree) bottom
    where
        tree = (arr ! y) ! x
        height = numElements arr
        width = numElements (arr ! 0)
        (left, right, top, bottom) = lineOfSight (width, height) (x, y)

takeWhileLeq :: Ord a => a -> [a] -> [a]
takeWhileLeq _ [] = []
takeWhileLeq y (x:xs)
    | x < y = x : takeWhileLeq y xs
    | otherwise = [x]

scenicScore :: Forest -> Coord -> Int
scenicScore arr (x, y) = product . map length $
    [takeWhileLeq tree $ map (\(x, y) -> (arr ! y) ! x) left,
     takeWhileLeq tree $ map (\(x, y) -> (arr ! y) ! x) right,
     takeWhileLeq tree $ map (\(x, y) -> (arr ! y) ! x) top,
     takeWhileLeq tree $ map (\(x, y) -> (arr ! y) ! x) bottom]
    where
        tree = (arr ! y) ! x
        height = numElements arr
        width = numElements (arr ! 0)
        (left, right, top, bottom) = lineOfSight (width, height) (x, y)

p2 :: Forest -> Int
p2 arr = (maximum . map (scenicScore arr)) interior
    where
        height = numElements arr
        width = numElements (arr ! 0)
        interior = [(x, y) | x <- [1..width-2], y <- [1..height-2]]

p1 :: Forest -> Int
p1 arr = height * 2 + width * 2 - 4 + length (filter (isVisible arr) interior)
    where
        height = numElements arr
        width = numElements (arr ! 0)
        interior = [(x, y) | x <- [1..width-2], y <- [1..height-2]]

day8 :: IO ()
day8 = do
    arr <- day8input
    let
        height = numElements arr
        width = numElements (arr ! 0)
        ixs = [(x, y) | x <- [1..width-2], y <- [1..height-2]]
    putStrLn "⭐⭐ Day 8 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (p1 arr)
    putStrLn $ "Part 2: " ++ show (p2 arr)

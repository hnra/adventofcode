module Main where

import Data.Map (Map, (!?))
import qualified Data.Map as Map

type Memory = Map (Int, Int) Int
type MemKey = (Int, Int)
type Coords = (Int, Int)

findSide :: Int -> Int -> Int
findSide width num =
  head [x | x <- map (\k -> if width^2 - (k * width - k) < num then k else 0) [1..4], x /= 0]

gridWidthIncl :: Int -> Int
gridWidthIncl num = head [x | x <- [1..], mod x 2 /= 0, x^2 >= num]

getCoord :: Int -> Int -> Int -> Coords
getCoord side width num
  | side == 1 = (width - (max' - num), width)
  | side == 2 = (1, (width - ((max' - width + 1) - num)))
  | side == 3 = ((max' - 2 * (width - 1)) - num + 1, 1)
  | side == 4 = (width, (max' - 3 * (width - 1)) - num + 1)
  | otherwise = (0,0)
  where
    max' = width^2

getXpos :: Int -> Coords
getXpos w = (a,a)
  where a = ceiling $ (fromIntegral w) / 2

getSteps :: Coords -> Coords -> Int
getSteps xpos coord = x + y
  where
    x = abs $ fst coord - fst xpos
    y = abs $ snd coord - snd xpos

sumWithNothing :: [Maybe Int] -> Int
sumWithNothing [] = 0
sumWithNothing (Nothing:xs) = 0 + sumWithNothing xs
sumWithNothing (Just a:xs) = a + sumWithNothing xs

addValue :: Memory -> MemKey -> Memory
addValue memory (x,y) =
  Map.insert (x,y) value memory
  where
    value = sumWithNothing [memory !? (i,j) | i <- [x-1..x+1], j <- [y-1..y+1], (i,j) /= (x,y)]

findMax :: Memory -> (MemKey, Int)
findMax memory = Map.foldrWithKey (\key val result -> if val > snd result then (key, val) else result) ((0,0),0) memory

nextCoord :: Memory -> MemKey -> MemKey
nextCoord memory (x,y)
  | memory !? (x-1,y) /= Nothing && memory !? (x,y+1) == Nothing = (x,y+1)
  | memory !? (x,y-1) /= Nothing && memory !? (x-1,y) == Nothing = (x-1,y)
  | memory !? (x+1,y) /= Nothing && memory !? (x,y-1) == Nothing = (x,y-1)
  | memory !? (x,y+1) /= Nothing && memory !? (x+1,y) == Nothing = (x+1,y)
  | otherwise = (x+1,y)

findNumber :: Memory -> Int -> Int
findNumber memory num
  | snd (findMax memory) > num = snd $ findMax memory
  | otherwise = findNumber (addValue memory $ nextCoord memory $ fst $ findMax memory) num

findSmallestGreaterNum :: Int -> Int
findSmallestGreaterNum number =
  findNumber memory number
  where memory = Map.fromList [((0,0),1)]

main :: IO ()
main = do
  line <- getLine
  let num = read $ line
  let width = gridWidthIncl num
  let side = findSide width num
  let part1 = getSteps (getXpos width) (getCoord side width num)
  let part2 = findSmallestGreaterNum num
  putStrLn $ "Part 1 answer: " ++ (show part1)
  putStrLn $ "Part 2 answer: " ++ (show part2)

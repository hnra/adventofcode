module Main where

import Prelude hiding (length, maximum, tail, head, splitAt, (++), elem)
import qualified Prelude as P ((++))
import Data.Vector
import Debug.Trace

type Index = Int
type Blocks = Int

initialDis :: Vector Blocks
initialDis = fromList [0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11]

adjust :: (Int -> Int) -> Int -> Vector Int -> Vector Int
adjust f i xs =
  let (front, back) = splitAt i xs
      newElem = f (head back)
  in snoc front newElem ++ (tail back)

distribute :: Vector Blocks -> Index -> Blocks -> Vector Blocks
distribute xs _ 0 = xs
distribute xs i blocks
  | i < length xs =
      let newDist = adjust (+1) i xs
      in distribute newDist (i + 1) (blocks - 1)
  | otherwise = distribute xs 0 blocks

loop :: Vector (Vector Blocks) -> Vector Blocks -> Int -> Int
loop pDists cDist cnt
  | elem cDist pDists == True = cnt
  | otherwise =
    let i = maxIndex cDist
        blocks = cDist ! i
        newDist = distribute (adjust (const 0) i cDist) (i + 1) blocks
    in loop (cons cDist pDists) newDist (cnt + 1)

loop' :: Vector (Vector Blocks) -> Vector Blocks -> Int -> Int
loop' pDists cDist cnt
  | elem cDist pDists == True = loop mempty cDist 0
  | otherwise =
    let i = maxIndex cDist
        blocks = cDist ! i
        newDist = distribute (adjust (const 0) i cDist) (i + 1) blocks
    in loop' (cons cDist pDists) newDist (cnt + 1)

main :: IO ()
main = do
  putStrLn $ "Part 1: " P.++ (show $ loop empty initialDis 0)
  putStrLn $ "Part 2: " P.++ (show $ loop' mempty initialDis 0)

module Day10 (day10) where

import Utilities (getLines, tread, arr2d)
import qualified Data.Text as T
import GHC.Arr (readSTArray, writeSTArray, freezeSTArray, thawSTArray, STArray, elems, (!), bounds, Array)
import Control.Monad.ST (ST, runST)
import Data.List (intersperse)
import Control.Monad (foldM)

data Instruction = Addx Int | Noop

day10input :: IO [Instruction]
day10input = map parse <$> getLines "10"
    where
        parse l = case T.splitOn " " l of
            ("noop":_) -> Noop
            (_:amount:_) -> Addx (tread amount)
            _ -> error "Cannot parse"

data Computer = Computer [Int] [Instruction]

run :: Computer -> Computer
run c@(Computer reg []) = c
run (Computer rs@(r:_) (i:is)) = run c
    where
        c = case i of
            Addx am -> Computer ((r+am):r:rs) is
            Noop -> Computer (r:rs) is
run _ = error "Cannot run Computer"

newtype CRT s = CRT (STArray s (Int, Int) Char)

printArr :: Array (Int, Int) Char -> String
printArr arr = concat . intersperse "\n" $ map showRow [0..height]
    where
        ((_, _), (width, height)) = bounds arr
        showRow y = map (\x -> arr ! (x, y)) [0..width]

drawPixel :: CRT s -> ((Int, Int), Int) -> ST s (CRT s)
drawPixel crt@(CRT arr) ((x, y), r)  = do
    let char = if abs (x - r) < 2 then '█' else ' '
    writeSTArray arr (x, y) char
    pure crt

draw :: (Int, Int) -> [Int] -> String
draw (width, height) xs = runST $ do
    let pixels = zip [(x, y) | y <- [0..height-1], x <- [0..width-1]] xs
    crt <- thawSTArray (arr2d width height '#')
    (CRT starr) <- foldM drawPixel (CRT crt) pixels
    arr <- freezeSTArray starr
    pure $ printArr arr

day10 :: IO ()
day10 = do
    input <- day10input
    let
        pc = Computer [1] input
        (Computer xs _) = run pc
        xs' = reverse xs
    putStrLn "⭐⭐ Day 10 ⭐⭐"
    putStrLn $ "Part 1: " ++ show ((sum . map (\c -> (c + 1) * xs' !! c)) [19,59..219])
    putStrLn $ "Part 2:\n" ++ draw (40, 6) xs'

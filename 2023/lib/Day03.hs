module Day03 where

import Utilities (hm2d, getLinesS, adjacent)
import Data.HashMap.Strict (HashMap, (!?), (!))
import qualified Data.HashMap.Strict as HM
import Data.Char (isDigit)
import Data.List (nub)

type Schematic = HashMap (Int, Int) Char

-- | A horizontal range in the schema.
data Range = Range { _y :: Int, _xMin :: Int, _xMax :: Int }
  deriving (Eq, Show)

day3input :: IO Schematic
day3input = do
  lines <- getLinesS "03"
  return $ hm2d lines

isSymbol :: Char -> Bool
isSymbol '.' = False
isSymbol c = (not . isDigit) c

isDigitInSchema s (x, y) = case s !? (x, y) of
  Nothing -> False
  Just c -> isDigit c

-- | Finds the range of the number which covers the digit at the coordinate.
numberRange :: Schematic -> (Int, Int) -> Range
numberRange s (x, y) = Range y xMin xMax
  where
    f x = isDigitInSchema s (x, y)
    xMin = (last . takeWhile f) [x,x-1..]
    xMax = (last . takeWhile f) [x,x+1..]

-- | Converts a range into a number.
number :: Schematic -> Range -> Int
number s (Range y xMin xMax) = read [ s ! (x, y) | x <- [xMin..xMax] ]

part1 :: Schematic -> Int
part1 s = sum numbers
  where
    symbols = (HM.keys . HM.filter isSymbol) s
    digitCandidates = concatMap adjacent symbols
    digits = filter (isDigitInSchema s) digitCandidates
    ranges = (nub . map (numberRange s)) digits
    numbers = map (number s) ranges

day3 :: IO ()
day3 = do
  putStrLn "⭐⭐ Day 3 ⭐⭐"
  input <- day3input
  let p1 = part1 input
  putStrLn $ "Part 1: " ++ show p1

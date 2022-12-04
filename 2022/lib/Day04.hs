module Day04 (day4) where

import Utilities ( tread, getLines )
import Data.List (intersect)
import Data.Text (Text)
import qualified Data.Text as T

parseRange :: Text -> [Int]
parseRange s = [(tread low) .. (tread high)]
    where
        (low:high:_) = T.splitOn "-" s

day04input :: IO [[[Int]]]
day04input = do
    input <- getLines "04"
    return $ map (map parseRange . T.splitOn ",") input

allOverlap :: [[Int]] -> Bool
allOverlap (a1:a2:_) = isec == length a1 || isec == length a2
    where
        isec = length $ intersect a1 a2
allOverlap _ = False

p1 :: [[[Int]]] -> Int
p1 = sum . map (fromEnum . allOverlap)

anyOverlap :: [[Int]] -> Bool
anyOverlap (a1:a2:_) = not (null (a1 `intersect` a2))
anyOverlap _ = False

p2 :: [[[Int]]] -> Int
p2 = sum . map (fromEnum . anyOverlap)

day4 :: IO ()
day4 = do
    input <- day04input
    putStrLn "⭐⭐ Day 4 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (p1 input)
    putStrLn $ "Part 2: " ++ show (p2 input)

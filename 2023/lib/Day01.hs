module Day01 where 

import Utilities (getLinesS)
import Data.Char (isDigit)

day1input :: IO [String]
day1input = getLinesS "01"

digits :: String -> String
digits = filter isDigit

calibrationValues :: [String] -> [Int]
calibrationValues = map (read .firstAndLast)
  where firstAndLast l = head l:last l:""

day1 :: IO ()
day1 = do
  putStrLn "⭐⭐ Day 1 ⭐⭐"
  input <- day1input
  let p1 = (sum . calibrationValues . map digits) input
  putStrLn $ "Part 1: " ++ show p1

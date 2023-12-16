module Day01 where 

import Utilities (getLinesS)
import Data.Char (isDigit)
import Data.Text (replace, pack, unpack, Text)

day1input :: IO [String]
day1input = getLinesS "01"

digits :: String -> String
digits = filter isDigit

calibrations :: [String] -> [Int]
calibrations = map (read . firstAndLast)
  where firstAndLast l = head l:last l:""

replacers :: [Text -> Text]
replacers =
  [ replace "one" "o1e"
  , replace "two" "t2o"
  , replace "three" "th3ee"
  , replace "four" "fo4r"
  , replace "five" "f5ve"
  , replace "six" "s6x"
  , replace "seven" "se7en"
  , replace "eight" "ei8ht"
  , replace "nine" "ni9e"
  , replace "zero" "ze0o"
  ]

-- | Converts a string containg (possibly overlapping) digits as words and
-- digit chars to a string of only digits.
-- @wordsToDigits "eightwo3sevenine" == "82379"@
wordsToDigits :: String -> String
wordsToDigits line = digits (unpack tline)
  where 
    tline = foldr ($) (pack line) replacers

day1 :: IO ()
day1 = do
  putStrLn "⭐⭐ Day 1 ⭐⭐"
  input <- day1input
  let p1 = (sum . calibrations . map digits) input
  let p2 = (sum . calibrations . map wordsToDigits) input
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

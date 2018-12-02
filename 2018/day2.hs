module Main where

countChar :: Char -> String -> Int
countChar _ [] = 0
countChar c (x:xs)
  | c == x = 1 + countChar c xs
  | otherwise = countChar c xs

remChar :: Char -> String -> String
remChar _ [] = []
remChar c (x:xs)
  | c == x = remChar c xs
  | otherwise = x : (remChar c xs)

dupsAndTrips :: String -> (Int, Int)
dupsAndTrips [] = (0, 0)
dupsAndTrips (x:xs) =
  let cnt =  countChar x (x:xs)
      newStr = remChar x xs
      (dups, trips) = dupsAndTrips newStr
  in case cnt of
       3 -> (dups, 1)
       2 -> (1, trips)
       _ -> (dups, trips)

sumTup :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumTup (x, y) (x', y') = (x + x', y + y')

main :: IO ()
main = do
  file <- words <$> (readFile "day2_input")
  let (d, t) = foldr1 sumTup $ map dupsAndTrips file
  putStrLn $ "Part 1: " ++ (show $ d * t)


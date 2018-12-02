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

isSimilar :: String -> String -> Bool
isSimilar xs ys = if length xs == length ys then go 0 xs ys else False
    where
      go diffs [] []
        | diffs > 1 = False
        | otherwise = True
      go diffs (x:xs) (y:ys)
        | diffs > 1 = False
        | x == y = go diffs xs ys
        | otherwise = go (diffs + 1) xs ys

findSimilar :: [String] -> String
findSimilar (x:xs) =
  let similars = filter (isSimilar x) xs
  in
    case similars of
      [] -> findSimilar xs
      (y:_) -> zipWith (\x y -> if x == y then x else '_') x y

main :: IO ()
main = do
  file <- words <$> (readFile "day2_input")
  let (d, t) = foldr1 sumTup $ map dupsAndTrips file
  putStrLn $ "Part 1: " ++ (show $ d * t)
  putStrLn $ "Part 2: " ++ (show $ findSimilar file)

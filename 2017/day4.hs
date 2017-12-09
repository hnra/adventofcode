module Main where

getInput :: IO [[String]]
getInput = do
  input <- getLine
  let row = words input
  if length input > 0 then do
    rows <- getInput
    return (row : rows)
  else
    return [row]

cntSameWords :: [String] -> Int
cntSameWords wrds =
  foldr (\x res -> if x /= 1 then res+1 else res) 0 cntedWrds
  where
    cntedWrds = map (\k -> foldr (\x res -> if x==k then res + 1 else res) 0 wrds) wrds

passCnt :: [[String]] -> Int
passCnt phrases =
  foldr (\x res -> if (cntSameWords x == 0) then res + 1 else res) 0 phrases

countLetter :: String -> Char -> Int
countLetter word letter =
  foldr (\k res -> if k then res + 1 else res) 0 sameLetters
  where
    sameLetters = map (==letter) word

similarWord :: String -> String -> Bool
similarWord word1 word2
  | length word1 == length word2 = if letterDiff == 0 then True else False
  | otherwise = False
  where letterDiff = sum $ zipWith (-) (map (countLetter word1) word2) (map (countLetter word1) word1)

countSimilarWords :: [String] -> [Int]
countSimilarWords wrds =
  map (\word -> foldr (\w res -> if similarWord w word then res + 1 else res) 0 wrds) wrds

uniqPhrase :: [[Int]] -> [Bool]
uniqPhrase countedList = map (any (/=1)) countedList

passCnt' :: [[String]] -> Int
passCnt' phrases =
  foldr (\nUniq res -> if nUniq then res else res + 1) 0 (uniqPhrase cntedPhrases)
  where
    cntedPhrases = map countSimilarWords phrases

main :: IO ()
main = do
  input <- getInput
  let test = take ((length input) - 1) input
  let part1 = passCnt test
  let part2 = passCnt' test
  putStrLn $ "Part 1 answer: " ++ (show part1)
  putStrLn $ "Part 2 answer: " ++ (show part2)

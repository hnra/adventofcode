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

main :: IO ()
main = do
  input <- getInput
  let test = take ((length input) - 1) input
  let part1 = passCnt test
  putStrLn $ "Part 1 answer: " ++ (show part1)

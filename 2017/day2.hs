strToInt :: String -> Int
strToInt [] = 0
strToInt xs = read xs

getInput :: IO [[Int]]
getInput = do
  input <- getLine
  let row = map strToInt (words input)
  if length input > 0 then do
    rows <- getInput
    return (row : rows)
  else
    return [row]

getChecksum :: [[Int]] -> Int
getChecksum [] = 0
getChecksum (x:xs) = (rowChecksum x) + getChecksum xs
    where
      rowChecksum :: [Int] -> Int
      rowChecksum [] = 0
      rowChecksum ys = (maximum ys) - (minimum ys)

getEvenDiv :: Int -> [Int] -> Int
getEvenDiv _ [] = 0
getEvenDiv x (y:ys)
  | mod x y == 0 = div x y
  | mod y x == 0 = div y x
  | otherwise = getEvenDiv x ys

sumEven :: [Int] -> Int
sumEven [] = 0
sumEven (x:xs) = getEvenDiv x xs + sumEven xs

getChecksum' :: [[Int]] -> Int
getChecksum' [] = 0
getChecksum' (x:xs) = sumEven x + getChecksum' xs

main :: IO ()
main = do
  putStrLn "Chose part 1 or 2"
  mode <- getLine
  if mode == "1" then do
    putStrLn "Part 1 input:"
    sheet <- getInput
    let checksum = getChecksum sheet
    putStrLn $ show checksum
  else if mode == "2" then do
    putStrLn "Part 2 input:"
    sheet <- getInput
    let checksum = getChecksum' sheet
    putStrLn $ show checksum
  else
    putStrLn "No mode selected"

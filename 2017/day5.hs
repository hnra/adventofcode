module Main where
  type Memory = [Int]
  type PC = Int

  getInput :: IO [Int]
  getInput = do
    input <- getLine
    let num = read input
    if length input > 0 then do
      nums <- getInput
      return (num : nums)
    else
      return []

  replaceAt :: [Int] -> Int -> Int -> [Int]
  replaceAt ary index value =
    fst splitAry ++ value : (tail $ snd splitAry)
    where splitAry = (splitAt index ary)

  run :: Memory -> PC -> Maybe (Memory, PC)
  run mem pc =
    if pc + (mem !! pc) < length mem then
      Just (replaceAt mem pc (mem !! pc + 1), pc + mem !! pc)
    else
      Nothing

  cntSteps :: Memory -> Int -> Int -> Int
  cntSteps mem pc steps =
    case run mem pc of
      Just (mem',pc') -> cntSteps mem' pc' (steps + 1)
      Nothing -> steps

  main :: IO ()
  main = do
    input <- getInput
    let part1 = cntSteps input 0 1
    putStrLn $ "Part 1: " ++ (show part1)

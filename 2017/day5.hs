module Main where
  import Data.Sequence (Seq, adjust', index)
  import qualified Data.Sequence as DS

  type Memory = Seq Int
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

  run :: Memory -> PC -> Maybe (Memory, PC)
  run mem pc =
    if pc + (index mem pc) < DS.length mem then
      Just (adjust' (+1) pc mem, pc + (index mem pc))
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
    let seqI = DS.fromList input
    let part1 = cntSteps seqI 0 1
    putStrLn $ "Part 1: " ++ (show part1)

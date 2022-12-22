module Day21 where

import Utilities (getInput, unreachable)
import Data.Attoparsec.Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

data Monkey = Number Double | Op Char String String
    deriving (Eq, Show)

type MonkeyTree = HashMap String Monkey

pOp = do
    left <- many' letter
    op <- skipSpace *> anyChar <* skipSpace
    right <- many' letter
    return $ Op op left right

pMonkey = do
    monkeyName <- many' letter <* string ": "
    digitOrOp <- double `eitherP` pOp
    return (monkeyName, case digitOrOp of
        Left d -> Number d
        Right op -> op)

day21input :: IO MonkeyTree
day21input = do
    input <- getInput "21"
    let (Right monkeys) = parseOnly (many' (pMonkey <* endOfLine)) input
    return (HM.fromList monkeys)

resolve :: MonkeyTree -> String -> Double
resolve tree name =
    case tree HM.! name of
        Number d -> d
        Op '+' a b -> resolve tree a + resolve tree b
        Op '-' a b -> resolve tree a - resolve tree b
        Op '*' a b -> resolve tree a * resolve tree b
        Op '/' a b -> resolve tree a / resolve tree b
        _ -> unreachable

solveRoot :: MonkeyTree -> (Double, Double) -> Integer
solveRoot tree (low, high)
    | head deltas == 0 = (toInteger . round) mid
    | low == high = error "Failed to solve root"
    | abs (deltas !! 1) < abs (deltas !! 2) = solveRoot tree (low, mid)
    | otherwise = solveRoot tree (mid, high)
    where
        mid = low + fromIntegral (round $ (high - low) / 2)
        midLow = low + fromIntegral (round $ (mid - low) / 2)
        midHigh = mid + fromIntegral (round $ (high - mid) / 2)
        deltas = map (\d -> resolve (HM.insert "humn" (Number d) tree) "root") [mid, midLow, midHigh]

day21 = do
    input <- day21input
    let
        part1 = toInteger $ round $ resolve input "root"
        part2 = solveRoot 
            (HM.adjust (\(Op _ a b) -> Op '-' a b) "root" input)
            (0, fromIntegral (maxBound :: Int))
    putStrLn "⭐⭐ Day 21 ⭐⭐"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2

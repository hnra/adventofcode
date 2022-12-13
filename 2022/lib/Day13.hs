module Day13 where

import Utilities (getInput, eithersToList, unreachable)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Data.Maybe (fromMaybe)

data Input = List [Input] | Signal Int deriving (Eq)

pList :: Parser Input
pList = do
    char '['
    inputOrSignals <- sepBy' (pList `eitherP` (decimal :: Parser Int)) (char ',')
    char ']'
    return (List (eithersToList id Signal inputOrSignals))

pPairs :: Parser (Input, Input)
pPairs = do
    left <- pList
    endOfLine
    right <- pList
    many' endOfLine
    return (left, right)

day13input :: IO [(Input, Input)]
day13input = do
    input <- getInput "13"
    let res = parseOnly (many' pPairs) input
    return $ case res of
        Left err -> error ""
        Right pairs -> pairs

rightOrder :: [Input] -> [Input] -> Maybe Bool
rightOrder ((Signal l):ls) ((Signal r):rs)
    | l < r = Just True
    | l == r = rightOrder ls rs
    | l > r = Just False
    | otherwise = unreachable
rightOrder ((List l):ls) ((List r):rs) =
    case rightOrder l r of
        Just b -> Just b
        Nothing -> rightOrder ls rs
rightOrder ((Signal l):ls) rs@((List _):_) = rightOrder (List [Signal l] : ls) rs
rightOrder ls@((List _):_) ((Signal r):rs) = rightOrder ls (List [Signal r] : rs)
rightOrder [] (_:_) = Just True
rightOrder (_:_) [] = Just False
rightOrder [] [] = Nothing

run :: Input -> Input -> Bool
run (List l) (List r) = fromMaybe unreachable (rightOrder l r)
run _ _ = unreachable

reorder :: [Input] -> [Input]
reorder [] = []
reorder (i:is) = reorder lefts ++ i:reorder rights
    where lefts = filter (`run` i) is
          rights = filter (run i) is

day13 :: IO ()
day13 = do
    input <- day13input
    let
        dividers = [List [List [Signal 2]], List [List [Signal 6]]]
        part2input = concatMap (\(l, r) -> [l, r]) input ++ dividers
        part1 = (sum . map fst . filter snd)
            $ zip [1..] (map (uncurry run) input)
        part2 = (product . map fst . filter ((`elem` dividers) . snd)) 
            $ zip [1..] (reorder part2input)
    putStrLn "⭐⭐ Day 13 ⭐⭐"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2

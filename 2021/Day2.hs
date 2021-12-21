module Main where

import Text.Read
import Data.Maybe
import Text.Printf

data Direction = Forward | Down | Up
data Step = Step Direction Integer

parseDir :: String -> Maybe Direction
parseDir "forward" = Just Forward
parseDir "down" = Just Down
parseDir "up" = Just Up
parseDir _ = Nothing

parseStep :: String -> Maybe Step
parseStep s =
    case words s of
        (x:y:_) -> do
            dir <- parseDir x
            amount <- readMaybe y
            return $ Step dir amount
        _ -> Nothing

input :: IO [Step]
input = do
    steps <- readFile "inputs/day2.txt"
    return $ catMaybes $ fmap parseStep (lines steps)

data State = State Integer Integer

p1Run :: State -> Step -> State
p1Run (State h d) (Step Forward a) = State (h + a)  d
p1Run (State h d) (Step Up a)      = State  h      (d - a)
p1Run (State h d) (Step Down a)    = State  h      (d + a)

p1 :: [Step] -> Integer
p1 steps =
    case foldl p1Run (State 0 0) steps of
        (State h d) -> h * d

data State2 = State2 Integer Integer Integer

p2Run :: State2 -> Step -> State2
p2Run (State2 h d s) (Step Forward a) = State2 (h + a) (d + a * s)  s 
p2Run (State2 h d s) (Step Up a)      = State2  h       d          (s - a)
p2Run (State2 h d s) (Step Down a)    = State2  h       d          (s + a) 

p2 :: [Step] -> Integer
p2 steps =
    case foldl p2Run (State2 0 0 0) steps of
        (State2 h d _) -> h * d

main = do
    print "Day 2"
    steps <- input
    printf "Problem 1: %i\n" (p1 steps)
    printf "Problem 2: %i\n" (p2 steps)


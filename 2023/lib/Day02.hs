module Day02 where

import Utilities (unreachable, getInput)
import Data.Text (Text)
import Data.Attoparsec.Text (Parser, decimal, space, many', sepBy', parseOnly, endOfLine, string)
import Control.Applicative ((<|>))

-- Data types
data Color = Green | Blue | Red
  deriving (Eq, Show)

data Draw = Draw { _cnt :: Int, _color :: Color } 
  deriving (Eq, Show)

data Set = Set { _red :: Int, _green :: Int, _blue :: Int }
  deriving (Eq, Show)

data Game = Game Int [Set]
  deriving (Eq, Show)

toSet :: [Draw] -> Set
toSet = foldr go (Set 0 0 0)
  where
    go :: Draw -> Set -> Set
    go (Draw cnt Red) s = s { _red = cnt }
    go (Draw cnt Green) s = s { _green = cnt }
    go (Draw cnt Blue) s = s { _blue = cnt }

-- Parsing of input
day2input :: IO [Game]
day2input = do
  input <- getInput "02"
  case parseOnly games input of
    Left _ -> unreachable
    Right gs -> return gs

games :: Parser [Game]
games = many' game

game :: Parser Game
game = do
  gameId <- "Game " *> decimal <* ": "
  sets' <- sets
  endOfLine
  return $ Game gameId (map toSet sets')

sets :: Parser [[Draw]]
sets = draws `sepBy'` "; "

draws :: Parser [Draw]
draws = draw `sepBy'` ", "

draw :: Parser Draw
draw = do
  cnt <- decimal <* space
  color <- red <|> green <|> blue
  return $ Draw cnt color

red :: Parser Color
red = string "red" >> (return Red)

blue :: Parser Color
blue = string "blue" >> (return Blue)

green :: Parser Color
green = string "green" >> (return Green)

-- Part 1
maxSet :: Set
maxSet = Set 12 13 14

instance Ord Set where
  (Set r g b) <= (Set r' g' b') = r <= r' && g <= g' && b <= b'

isAllowedGame :: Game -> Bool
isAllowedGame (Game _ sets) = all (<=maxSet) sets

part1 :: [Game] -> Int
part1 [] = 0
part1 (g@(Game gameId _):gs)
  | isAllowedGame g = gameId + part1 gs
  | otherwise = part1 gs

-- Part 2
minSet :: Game -> Set
minSet (Game _ sets) = Set (max _red) (max _green) (max _blue)
  where
    max :: (Set -> Int) -> Int
    max = maximum . (flip map) sets

setPower :: Set -> Int
setPower (Set r g b) = r * g * b

part2 :: [Game] -> Int
part2 = sum . (map (setPower . minSet))

day2 :: IO ()
day2 = do
  putStrLn "⭐⭐ Day 2 ⭐⭐"
  gs <- day2input
  let p1 = part1 gs
  let p2 = part2 gs
  putStrLn $ "Part 1: " ++ show p1
  putStrLn $ "Part 2: " ++ show p2

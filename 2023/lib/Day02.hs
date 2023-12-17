module Day02 where

import Utilities (unreachable, getInput)
import Data.Text (Text)
import Data.Attoparsec.Text (Parser, decimal, space, many', sepBy', parseOnly, endOfLine)
import Control.Applicative ((<|>))

-- Data types
data Color = Green | Blue | Red
  deriving (Eq, Show)

data Draw = Draw { _cnt :: Int, _color :: Color } 
  deriving (Eq, Show)

data Game = Game Int [[Draw]]
  deriving (Eq, Show)

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
  Game gameId <$> sets <* endOfLine

sets :: Parser [[Draw]]
sets = draws `sepBy'` "; "

draws :: Parser [Draw]
draws = draw `sepBy'` ", "

draw :: Parser Draw
draw = do
  cnt <- decimal <* space
  color <- "red" <|> "green" <|> "blue"
  return $ Draw cnt (readColor color)

readColor :: Text -> Color
readColor "red" = Red
readColor "blue" = Blue
readColor "green" = Green
readColor _ = unreachable

-- Part 1
maxSet :: [Draw]
maxSet = [Draw 12 Red, Draw 13 Green, Draw 14 Blue]

isAllowed :: Draw -> Draw -> Bool
isAllowed (Draw cnt c') (Draw maxCnt c)
  | c == c' = cnt <= maxCnt
  | otherwise = True

isAllowedSet :: [Draw] -> Bool
isAllowedSet = foldr (\d -> (&&) (all (isAllowed d) maxSet)) True

isAllowedGame :: Game -> Bool
isAllowedGame (Game _ sets) = all isAllowedSet sets

part1 :: [Game] -> Int
part1 [] = 0
part1 (g@(Game gameId _):gs)
  | isAllowedGame g = gameId + part1 gs
  | otherwise = part1 gs

-- Part 2
minSet :: Game -> [Draw]
minSet (Game _ ds) =
  [ Draw (maxCnt reds) Red
  , Draw (maxCnt greens) Green
  , Draw (maxCnt blues)  Blue
  ]
  where
    withColor c = ((map _cnt) . (filter ((==c) . _color)) . concat) ds
    maxCnt [] = 0
    maxCnt xs = maximum xs
    reds = withColor Red
    greens = withColor Green
    blues = withColor Blue

setPower :: [Draw] -> Int
setPower = product . (map _cnt)

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

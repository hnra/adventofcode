module Day16 where

import Utilities (getInput)
import Data.Attoparsec.Text hiding (take)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (foldl1')

import Debug.Trace
import Control.Applicative ((<|>))

data Valve = Valve {
    valveId :: String,
    rate :: Int,
    tunnels :: [String]
} deriving Show

plural = string "s " <|> string " "

pLine :: Parser Valve
pLine = do
    valveId <- string "Valve " *> many' letter
    rate <- string " has flow rate=" *> decimal
    string "; tunnel" *> plural *> string "lead" *> plural *> string "to valve" *> plural
    tunnels <- many' letter `sepBy'` string ", "
    endOfLine
    return $ Valve valveId rate tunnels

day16input = do
    input <- getInput "16"
    let (Right valves) = parseOnly (many' pLine) input
    return valves

type Unopened = HashSet String
data Path = Move String | Open String deriving Show

type InputMap = HashMap String Valve
type DistanceMap = HashMap (String, String) Int

day16 = do
    putStrLn "Day 16"

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString.Char8
import Data.List
import qualified Data.HashMap.Lazy as HM

data Action =
    BeginShift Int
  | FallAsleep
  | WakeUp
  deriving (Eq, Show)

data Record = Record {
  _date :: Int,
  _time :: (Int, Int),
  _action :: Action
} deriving Eq

data Guard = Guard {
  _id :: Int,
  _ms :: [Int]
} deriving (Eq, Show)

instance Show Record where
  show (Record date (h,m) action) =
    (show date) ++ " " ++ (show h) ++ ":" ++ (show m) ++ " - " ++ (show action) ++ "\n"

instance Ord Record where
  compare (Record date (h,m) _) (Record date' (h', m') _)
    | date == date' && h == h' = compare m m'
    | date == date' = compare h h'
    | otherwise = compare date date'

parseAction :: Parser Action
parseAction = do
  firstChar <- anyChar
  case firstChar of
    'f' -> return FallAsleep
    'w' -> return WakeUp
    _   -> do
      string "uard #"
      id <- decimal
      return $ BeginShift id

parseRecord :: Parser Record
parseRecord = do
  char '['
  year <- many' digit
  char '-'
  month <- many' digit
  char '-'
  day <- many' digit
  space
  hour <- decimal
  char ':'
  minute <- decimal
  char ']'
  space
  action <- parseAction
  manyTill anyChar endOfLine
  return $ Record (read $ year ++ month ++ day) (hour, minute) action

parseRecords :: Parser [Record]
parseRecords = do
  records <- many' parseRecord
  return records

hmToGuards :: HM.HashMap Int [Int] -> [Guard]
hmToGuards hm =
  HM.foldrWithKey (\k v acc -> (Guard k v):acc) [] hm

getGuards :: [Record] -> [Guard]
getGuards records = go records HM.empty 0 0
  where
    go [] hm _ _ = hmToGuards hm
    go (r:rs) hm gid ptime =
      case _action r of
        BeginShift newGid -> go rs hm newGid 0
        FallAsleep -> go rs hm gid (snd $ _time r)
        WakeUp -> go rs (HM.insertWith (++) gid [ptime..(snd $ _time r)] hm) gid 0

findMinute :: Guard -> (Int, Int)
findMinute (Guard _ mins) =
  let minmap = foldr (\m acc -> HM.insertWith (+) m 1 acc) HM.empty mins
  in HM.foldrWithKey (\k v (m, count) -> if v >= count then (k, v) else (m, count)) (0,0) minmap

main :: IO ()
main = do
  file <- B.readFile "day4_input"
  case parseOnly parseRecords file of
    Left _ -> putStrLn "Failed to parse"
    Right recs' -> do
      let recs = sort recs'
          guards = getGuards recs
          guard = foldr (\g g' -> if (length $ _ms g) > (length $ _ms g') then g else g') (Guard (-1) []) guards
          (m, _) = findMinute guard
      putStrLn $ "Part 1: Guard #" ++ (show $ _id guard) ++ " Minute: " ++ (show m) ++ " Result: " ++ (show $ m * (_id guard)) 

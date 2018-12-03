{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (dropWhile, lines, readFile)
import Data.List (delete, findIndices, (\\))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, split, dropWhile, lines, readFile)
import Data.Attoparsec.ByteString (Parser, string, many')
import Data.Attoparsec.ByteString.Char8 (letter_ascii, decimal, atEnd, takeByteString, parseOnly)

data Program =
  Program {
      _name :: ByteString
    , _weight :: Int
    , _sub :: [ByteString]
    }
  deriving (Eq, Show)

data Tower =
  Tower Program [Tower]
  deriving (Eq, Show)

find :: (a -> Bool) -> [a] -> a
find f xs = xs !! (head $ findIndices f xs)

removeDups :: Eq a => [a] -> [a]
removeDups xs = fst $ foldr (\x (res, dups) ->
  if elem x dups
    then (res, dups)
    else
      if elem x res
        then (delete x res, x : dups)
        else (x : res, dups)) ([], []) xs

parseProgram :: Parser Program
parseProgram = do
  name <- many' letter_ascii
  string " ("
  weight <- decimal
  string ")"
  hasNoSubProgs <- atEnd
  if hasNoSubProgs == True
    then
      return $ Program (pack name) weight []
    else
      do
        string " -> "
        progs <- takeByteString
        return $ Program (pack name) weight (fmap (dropWhile (==' ')) $ split ',' progs)

findBase :: [Program] -> Program
findBase progs = find (\(Program n _ _) -> n == name) progs
  where
    name = findBaseName progs
    findBaseName progs = head $ removeDups $ concat $ map (\(Program n _ ps) -> n : ps) progs

findProgs :: [Program] -> [ByteString] -> [Program]
findProgs progs names =
  foldr (\p@(Program n _ _) acc -> if elem n names then p : acc else acc) [] progs

buildTower :: Program -> [Program] -> Tower
buildTower b@(Program n _ children) progs =
  let subtowers = map (\p -> buildTower p progs) (findProgs progs children)
  in Tower b subtowers

towerWeight :: Tower -> Int
towerWeight (Tower (Program _ w _) ts) = w + (foldr ((+) . towerWeight) 0 ts)

findHeavy :: Tower -> (Tower, Int)
findHeavy twrs = go twrs 0
  where
    go t@(Tower _ ts) corrW =
      let weights = map towerWeight ts
          uniqs = removeDups weights
          corrW' = head $ weights \\ uniqs
      in case uniqs of
          [] -> (t, corrW)
          _  -> go (find (\t -> towerWeight t == (head uniqs)) ts) corrW'

main :: IO ()
main = do
  file <- readFile "day7_input"
  let programs :: Either String [Program]
      programs = sequence $ fmap (parseOnly parseProgram) (lines file)
  case programs of
    Left err -> putStrLn "Couldn't parse the input"
    Right progs ->
      let baseProg = findBase progs
          tower = buildTower baseProg progs
          (t@(Tower (Program _ w _) _), corrW) = findHeavy tower
      in do
        putStrLn $ "Part 1: " ++ (show $ _name baseProg)
        putStrLn $ "Part 2: " ++ (show $ w - (towerWeight t - corrW))

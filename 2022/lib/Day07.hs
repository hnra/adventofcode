module Day07 (day7) where

import Utilities (getLines, tread)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Trans.State.Strict (State, get, put, execState)
import Data.List (foldl')

type Size = Integer
type Name = Text
type Path = [Name]
data File = File { fname :: Name, size :: Size }
data Directory = Directory { dname :: Name, files :: [File], dirs :: [Directory] }

type Output = Either File Name
data Command = Ls [Output] | Cd Name

type Computer = State (Path, Directory) ()

flatMapDir :: (Directory -> a) -> Directory -> [a]
flatMapDir f dir@(Directory _ _ ds) = f dir : foldMap (flatMapDir f) ds

parseOutput :: Text -> Output
parseOutput t =
    case T.splitOn " " t of
        ("dir":n:_) -> Right n
        (size:n:_) -> Left $ File n (tread size)
        _ -> error "Cannot parse output"

parse :: [Text] -> [Command]
parse [] = []
parse (t:ts)
    | "$ cd" `T.isPrefixOf` t = parseCd t : parse ts
    | "$ ls" `T.isPrefixOf` t = Ls (map parseOutput output) : parse rest
    | otherwise = error "Bad parsing"
      where
        isCmd = T.isPrefixOf "$"
        output = takeWhile (not . isCmd) ts
        rest = dropWhile (not . isCmd) ts
        parseCd t = Cd p
            where (_:_:p:_) = T.splitOn " " t

day7input :: IO [Command]
day7input = do
    input <- getLines "07"
    return $ parse input

insert :: Directory -> Output -> Directory
insert dir@(Directory n fs ds) (Right d)
    | d `elem` map dname ds = dir
    | otherwise = Directory n fs (Directory d [] []:ds)
insert dir@(Directory n fs ds) (Left (File fn s))
    | fn `elem` map fname fs = dir
    | otherwise = Directory n (File fn s:fs) ds

insertAt :: Path -> Output -> Directory -> Directory
insertAt [] _ _ = error "Cannot insert without a path"
insertAt [p] output dir@(Directory n fs ds)
    | p == n = insert dir output
    | otherwise = dir
insertAt (p:ps) output dir@(Directory n fs ds)
    | p == n = Directory n fs (map (insertAt ps output) ds)
    | otherwise = dir

command :: Command -> Computer
command c = do
    (cwd, root) <- get
    case c of
        Cd ".." -> put (init cwd, root)
        Cd "/" -> put (["/"], root)
        Cd p -> put (cwd ++ [p], root)
        Ls output -> put (cwd, foldl' (flip (insertAt cwd)) root output)
    return ()

runPc :: [Command] -> (Path, Directory)
runPc cmds = execState (traverse command cmds) ([""], Directory "/" [] [])

dirSize :: Directory -> Integer
dirSize (Directory _ fs ds) = (sum . map dirSize) ds + (sum . map size) fs

part1 :: [Command] -> Integer
part1 cmds = (sum . filter (<=100000) . flatMapDir dirSize) ds
    where
        (_, ds) = runPc cmds

part2 :: [Command] -> Integer
part2 cmds = (minimum . filter (>=smallestRemove) . flatMapDir dirSize) ds
    where
        (_, ds) = runPc cmds
        used = dirSize ds
        unused = 70000000 - used
        smallestRemove = 30000000 - unused

day7 :: IO ()
day7 = do
    input <- day7input
    putStrLn "⭐⭐ Day 7 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

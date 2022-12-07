module Day07 where

import Utilities (getLines, tread)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Trans.State.Strict (State, get, put, execState)
import Data.List (foldl')

import Debug.Trace (trace)

type Size = Integer
type Name = Text
type Path = [Name]

data File = File Name Size
    deriving (Eq, Show)
type Output = Either File Name
data Directory = Directory Name [File] [Directory]
    deriving (Eq, Show)

fName (File n _) = n
fSize (File _ s) = s
dName (Directory n _ _) = n

flatMapDir :: (Directory -> a) -> Directory -> [a]
flatMapDir f dir@(Directory _ _ ds) = f dir : foldMap (flatMapDir f) ds

data Command = Ls [Output] | Cd Name
    deriving (Eq, Show)

parseCd :: Text -> Command
parseCd t = Cd p
    where (_:_:p:_) = T.splitOn " " t

parseOutput :: Text -> Output
parseOutput t =
    case T.splitOn " " t of
        ("dir":n:_) -> Right n
        (size:n:_) -> Left $ File n (tread size)

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

day7input :: IO [Command]
day7input = do
    input <- getLines "07"
    return $ parse input

type Computer = State (Path, Directory) ()

insert :: Directory -> Output -> Directory
insert dir@(Directory n fs ds) (Right d)
    | d `elem` map dName ds = dir
    | otherwise = Directory n fs (Directory d [] []:ds)
insert dir@(Directory n fs ds) (Left (File fn s))
    | fn `elem` map fName fs = dir
    | otherwise = Directory n (File fn s:fs) ds

insertAt :: Path -> Output -> Directory -> Directory
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
dirSize (Directory _ fs ds) = (sum . map dirSize) ds + (sum . map fSize) fs

part1 :: [Command] -> Integer
part1 cmds = (sum . filter (<=100000) . flatMapDir dirSize) ds
    where
        (cwd, ds) = runPc cmds

part2 :: [Command] -> Integer
part2 cmds = (minimum . filter (>=smallestRemove) . flatMapDir dirSize) ds
    where
        (cwd, ds) = runPc cmds
        used = dirSize ds
        unused = 70000000 - used
        smallestRemove = 30000000 - unused

day7 :: IO ()
day7 = do
    input <- day7input
    putStrLn "⭐⭐ Day 7 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)

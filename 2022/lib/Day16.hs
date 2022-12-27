module Day16 where

import Utilities (getInput)
import Data.Attoparsec.Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (sortOn, nub)
import Control.Applicative ((<|>))
import Data.Ord (Down(..))
import Data.Foldable (foldl')

data Valve = Valve { _rate :: Int, _tunnels :: [String] }

type Valves = HashMap String Valve
type Graph = HashMap String [(Int, String)]
type Opened = HashSet String

plural = string "s " <|> string " "

pLine :: Parser (String, Valve)
pLine = do
    valveId <- string "Valve " *> many' letter
    rate <- string " has flow rate=" *> decimal
    string "; tunnel" *> plural *> string "lead" *> plural *> string "to valve" *> plural
    tunnels <- many' letter `sepBy'` string ", "
    endOfLine
    return (valveId, Valve rate tunnels)

day16input :: IO Valves
day16input = do
    input <- getInput "16"
    let (Right valves) = parseOnly (many' pLine) input
    return $ HM.fromList valves

bfs :: Valves -> [(Int, String)] -> [(Int, String)] -> [(Int, String)]
bfs _ [] found = found
bfs valves ((i,vid):vids) found
    | vid `elem` map snd found = bfs valves vids found
    | otherwise = bfs valves (vids ++ next) ((i,vid):found)
    where
        ts = _tunnels (valves HM.! vid)
        next = zip (repeat (i+1)) ts

buildGraph :: Valves -> Graph
buildGraph valves =
    HM.fromList $
        map (\vid -> (vid, filter ((`elem` valves') . snd) (init (bfs valves [(0, vid)] [])))) ("AA":valves')
    where
        valves' = HM.keys (HM.filter ((>0) . _rate) valves)

rate :: Valves -> String -> Int
rate valves = _rate . (valves HM.!)

bestCase :: Valves -> Graph -> Opened -> Int -> String -> Int -> Int
bestCase valves graph opened minutes vid currRate =
    minutes * currRate + sum rates
    where
        nexts = unopened valves graph opened minutes vid
        rates = zipWith (\m (i, vid) -> max 0 (minutes - i - m) * rate valves vid) [1,3..] nexts

unopened :: Valves -> Graph -> Opened -> Int -> String -> [(Int, String)]
unopened valves graph opened minutes vid = sortOn (Down . potential valves minutes) unopened
    where
        reachable = filter ((<minutes) . fst) (graph HM.! vid)
        unopened = filter ((not . (`HS.member` opened)) . snd) reachable
        potential valves minutes (i, vid) = (minutes - i - 1) * rate valves vid

bestPath :: Valves -> Graph -> Opened -> Int -> String -> Int -> Int -> Int
bestPath valves graph opened minutes vid currRate best
    | minutes == 0 = 0
    | null nexts = minutes * currRate
    | otherwise = foldl' (\c -> max c . go c) 0 nexts
    where
        nexts = unopened valves graph opened minutes vid
        go best (i, vid)
            | best >= bc = 0
            | otherwise = currRate * (i + 1) + bestPath valves graph opened' minutes' vid newRate best
            where
                minutes' = minutes - i - 1
                opened' = HS.insert vid opened
                newRate = currRate + rate valves vid
                bc = currRate * (i + 1) + bestCase valves graph opened' minutes' vid newRate

combinations :: [(Int, String)] -> [(Int, String)] -> [((Int, String), (Int, String))]
combinations s1s s2s = notSame
    where
        combos = concatMap (\i -> zip s1s (drop i s2s) ++ zip (drop i s1s) s2s) [0..(length s1s)]
        notSame = nub $ filter (\(s1, s2) -> snd s1 /= snd s2) combos

bestDual :: Valves -> Graph -> Opened -> (Int, Int) -> (String, String) -> (Int, Int) -> (Int, Int)
bestDual valves graph opened (ms1, ms2) (vid1, vid2) (cr1, cr2)
    | null nexts1 && null nexts2 = (ms1 * cr1, ms2 * cr2)
    | null nexts1 = (ms1 * cr1, c2 + ms2 * cr2)
    | null nexts2 = (c1 + ms1 * cr1, ms2 * cr2)
    | null paths = (c1' + ms1 * cr1, c2' + ms2 * cr2)
    | otherwise = foldl' (\c -> max' c . go c) (0, 0) paths
    where
        nexts1 = unopened valves graph opened ms1 vid1
        nexts2 = unopened valves graph opened ms2 vid2
        paths = combinations nexts1 nexts2

        (i1', vid1') = head nexts1
        (i2', vid2') = head nexts2
        c1 = (ms1 - i1' - 1) * rate valves vid1'
        c2 = (ms2 - i2' - 1) * rate valves vid2'
        (c1', c2') = maxToTup c1 c2

        go (b1, b2) ((i1, vid1), (i2, vid2))
            | b1 + b2 >= bc1 + bc2 = (0, 0)
            | otherwise = (cr1 * (i1 + 1) + bd1, cr2 * (i2 + 1) + bd2)
            where
                opened' = HS.insert vid2 (HS.insert vid1 opened)
                ms1' = ms1 - i1 - 1
                ms2' = ms2 - i2 - 1
                cr1' = cr1 + rate valves vid1
                cr2' = cr2 + rate valves vid2
                bc1 = cr1 * (i1 + 1) + bestCase valves graph opened' ms1' vid1 cr1'
                bc2 = cr2 * (i2 + 1) + bestCase valves graph opened' ms2' vid2 cr2'
                (bd1, bd2) = bestDual valves graph opened' (ms1', ms2') (vid1, vid2) (cr1', cr2')
        maxToTup a b
            | a <= b = (0, b)
            | otherwise = (a, 0)
        max' (a, b) (c, d)
            | a + b <= c + d = (c, d)
            | otherwise = (a, b)

day16 :: IO ()
day16 = do
    valves <- day16input
    let graph = buildGraph valves
        part1 = bestPath valves graph HS.empty 30 "AA" 0 0
        part2 = uncurry (+) (bestDual valves graph HS.empty (26, 26) ("AA", "AA") (0, 0))
    putStrLn "⭐⭐ Day 16 ⭐⭐"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2

module Day16 where

import Utilities (getInput, unreachable)
import Data.Attoparsec.Text hiding (take)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (sortOn)
import Control.Applicative ((<|>))
import Data.Ord (Down(..))

import Debug.Trace

data Valve = Valve {
    _rate :: Int,
    _tunnels :: [String]
} deriving Show

type Valves = HashMap String Valve
type Graph = HashMap String [(Int, String)]

plural = string "s " <|> string " "

pLine :: Parser (String, Valve)
pLine = do
    valveId <- string "Valve " *> many' letter
    rate <- string " has flow rate=" *> decimal
    string "; tunnel" *> plural *> string "lead" *> plural *> string "to valve" *> plural
    tunnels <- many' letter `sepBy'` string ", "
    endOfLine
    return (valveId, Valve rate tunnels)

day16input = do
    input <- getInput "16"
    let (Right valves) = parseOnly (many' pLine) input
    return $ HM.fromList valves

bfs _ [] found = found
bfs valves ((i,vid):vids) found
    | vid `elem` map snd found = bfs valves vids found
    | otherwise = bfs valves (vids ++ bar) ((i,vid):found)
    where
        ts = _tunnels (valves HM.! vid)
        bar = zip (repeat (i+1)) ts

buildGraph valves = 
    HM.fromList $ 
        map (\vid -> (vid, filter ((`elem` valves') . snd) (init (bfs valves [(0, vid)] [])))) ("AA":valves')
    where
        valves' = HM.keys $ HM.filter ((>0) . _rate) valves

rate valves = _rate . (valves HM.!)
cost valves minutes (i, vid) = (minutes - i - 1) * rate valves vid

bestCase valves graph minutes opened vid currrate =
    minutes * currrate + sum (map (\(i, vid) -> (minutes - i) * rate valves vid) nexts)
    where nexts = unopened valves graph opened minutes vid

unopened :: Valves -> Graph -> HashSet String -> Int -> String -> [(Int, String)]
unopened valves graph opened minutes vid = sortOn (Down . cost valves minutes) unopened
    where
        reachable = filter ((<minutes) . fst) (graph HM.! vid)
        unopened = filter ((not . (`HS.member` opened)) . snd) reachable

bestPath :: Valves -> Graph -> Int -> HashSet String -> String -> Int -> Int -> Int
bestPath valves graph minutes opened vid currrate best
    | minutes == 0 = 0
    | best >= bc = 0
    | null nexts = minutes * currrate
    | otherwise = currrate * (i' + 1) + cost'
    where
        bc = bestCase valves graph minutes opened vid currrate
        nexts = unopened valves graph opened minutes vid
        go (i, vid) best =
            bestPath valves graph (minutes - i - 1) (HS.insert vid opened) vid (currrate + rate valves vid) best
        ivid = head nexts
        totCost (i, c) = currrate * (i + 1) + c
        (i', cost') = foldl (\ic ivid ->
            let cost' = go ivid (snd ic)
            in if totCost ic >= totCost (fst ivid, cost') then ic else (fst ivid, cost'))
            (fst ivid, go ivid 0) (tail nexts)

combinations :: [(Int, String)] -> [(Int, String)] -> [((Int, String), (Int, String))]
combinations s1s s2s = filter (\(s1, s2) -> snd s1 /= snd s2) combos
    where combos = concatMap (\i -> zip s1s (drop i s2s) ++ zip (drop i s1s) s2s) [0..(length s1s)]

bestDual valves graph (ms1, ms2) opened (vid1, vid2) (cr1, cr2)
    | null nexts1 && null nexts2 = ms1 * cr1 + ms2 * cr2
    | null nexts1 = c2 + ms1 * cr1 + ms2 * cr2
    | null nexts2 = c1 + ms1 * cr1 + ms2 * cr2
    | null paths = max c1 c2 + ms1 * cr1 + ms2 * cr2
    | otherwise = maximum $ map go paths
    where
        nexts1 = unopened valves graph opened ms1 vid1
        nexts2 = unopened valves graph opened ms2 vid2
        paths = combinations nexts1 nexts2

        (i1', vid1') = head nexts1
        (i2', vid2') = head nexts2
        c1 = (ms1 - i1' - 1) * rate valves vid1'
        c2 = (ms2 - i2' - 1) * rate valves vid2'

        go ((i1, vid1), (i2, vid2)) =
            cr1 * (i1 + 1) + cr2 * (i2 + 1) + bestDual valves graph (ms1-i1-1, ms2-i2-1) opened' (vid1, vid2) (cr1 + rate valves vid1, cr2 + rate valves vid2)
            where opened' = HS.insert vid2 (HS.insert vid1 opened)

day16 = do
    valves <- day16input
    putStrLn "Day 16"
    let graph = buildGraph valves
    print $ bestPath valves graph 30 HS.empty "AA" 0 0
    print $ bestDual valves graph (26, 26) HS.empty ("AA", "AA") (0, 0)

module Day12 where

import Utilities (hm2d)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Foldable ( foldl' )
import qualified Data.PQueue.Min as PQ
import Data.PQueue.Min (MinQueue)
import Data.List (transpose)

data Node = Node Index Int deriving Eq
instance Ord Node where
    (Node _ i) <= (Node _ i') = i < i'

type Index = (Int, Int)
type Graph = HashMap Index [Index]
type Dist = HashMap Index Int
type Queue = MinQueue Node
type Visited = HashSet Index

day12input :: IO (Graph, Index, Index, [Index])
day12input = do
    input <- lines <$> readFile "inputs/day12"
    let
        charMap = hm2d (transpose input)
        start = (head . HM.keys . HM.filter ( == 'S')) charMap
        end = (head . HM.keys . HM.filter (== 'E')) charMap
        as = (HM.keys . HM.filter (=='a')) charMap
        inverted = HM.map toHeight charMap
        graph = HM.mapWithKey (\k v -> neighbours k) inverted

        toHeight 'S' = fromEnum 'z'
        toHeight 'E' = fromEnum 'a'
        toHeight c = fromEnum 'a' + fromEnum 'z' - fromEnum c

        isNeighbour xy (nx, ny) =
            case HM.lookup (nx, ny) inverted of
                Just nc -> nc <= (inverted HM.! xy) + 1
                Nothing -> False

        neighbours (x, y) = filter
            (isNeighbour (x, y))
            [(x+1, y), (x-1,y), (x, y-1), (x, y+1)]
    return (graph, start, end, as)

dijkstra :: Index -> Graph -> Dist
dijkstra source graph = go dist queue HS.empty
    where
        dist = (HM.insert source 0 . HM.map (const (maxBound - 100))) graph
        queue = PQ.fromList $ map (uncurry Node) $ HM.toList dist

        go :: Dist -> Queue -> Visited -> Dist
        go dist queue visited
            | PQ.size unvisited == 0 = dist
            | otherwise = go dist' queue' visited'
            where
                unvisited = PQ.dropWhile (\(Node xy _) -> xy `HS.member` visited) queue
                ((Node u ud), unvisited') = PQ.deleteFindMin unvisited
                visited' = HS.insert u visited
                neighbours = (filter (not . (`HS.member` visited)) . (graph HM.!)) u

                (dist', queue') = foldl' (\(dist, queue) v ->
                    let alt = ud + 1
                    in if alt < dist HM.! v
                        then (HM.insert v alt dist, PQ.insert (Node v alt) queue)
                        else (dist, queue))
                    (dist, unvisited') neighbours

day12 :: IO ()
day12 = do
    (graph, start, end, as) <- day12input
    putStrLn "⭐⭐ Day 12 ⭐⭐"
    let dists = dijkstra end graph
        p1 = dists HM.! start
        p2 = (minimum . map (dists HM.!)) as
    putStrLn $ "Part 1: " ++ show p1
    putStrLn $ "Part 2: " ++ show p2

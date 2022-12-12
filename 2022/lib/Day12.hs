module Day12 where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.Foldable ( minimumBy, foldl' )

type Index = (Int, Int)
type Graph = HashMap Index (Char, [Index])

charToHeight 'S' = fromEnum 'a'
charToHeight 'E' = fromEnum 'z'
charToHeight c = fromEnum c

merge :: (a, [(b, c)]) -> [((a, b), c)]
merge (a, list) = map (\(b, c) -> ((a, b), c)) list

to2dHm :: [[a]] -> HashMap (Int, Int) a
to2dHm = HM.fromList . concatMap merge . zip [0..] . map (zip [0..])

invert :: Graph -> Graph
invert graph = HM.mapWithKey (\k (v, _) -> (invertChar v, neighbours k)) graph
    where
        invertChar :: Char -> Char
        invertChar 'S' = 'E'
        invertChar 'E' = 'S'
        invertChar c = toEnum $ fromEnum 'a' + (fromEnum 'z' - fromEnum c)

        isNeighbour xy (nx, ny) =
            case HM.lookup (nx, ny) graph of
                Just (nc, _) -> charToHeight (invertChar nc) <= c + 1
                Nothing -> False
                where
                    c = charToHeight $ invertChar $ fst $ graph HM.! xy

        neighbours (x, y) = filter
            (isNeighbour (x, y))
            [(x+1, y), (x-1,y), (x, y-1), (x, y+1)]

day12input :: IO (Graph, Index, Index)
day12input = do
    input <- lines <$> readFile "inputs/day12"
    let
        charArr = to2dHm input
        start = (head . HM.keys . HM.filter ( == 'S')) charArr
        end = (head . HM.keys . HM.filter (== 'E')) charArr
        graph = HM.mapWithKey (\k v -> (v, neighbours k)) charArr

        isNeighbour xy (nx, ny) =
            case HM.lookup (nx, ny) charArr of
                Just nc -> charToHeight nc <= c + 1
                Nothing -> False
                where
                    c = charToHeight $ charArr HM.! xy

        neighbours (x, y) = filter
            (isNeighbour (x, y))
            [(x+1, y), (x-1,y), (x, y-1), (x, y+1)]
    return (graph, start, end)

type Dist = HashMap Index Int
type Queue = HashSet Index

dijkstra :: Index -> Graph -> Dist
dijkstra source graph = go dist queue
    where
        dist = (HM.insert source 0 . HM.map (const (maxBound - 100))) graph
        queue = HS.fromList $ HM.keys graph

        go :: Dist -> Queue -> Dist
        go dist queue
            | HS.size queue == 0 = dist
            | otherwise = go dist' queue'
            where
                u@(ux, uy) = minimumBy (\xy xy' -> (dist HM.! xy) `compare` (dist HM.! xy')) queue
                queue' = HS.delete u queue
                (_, neighbours) = graph HM.! u
                dist' = foldl' (\dist v ->
                    let alt = dist HM.! u + 1
                    in if alt < dist HM.! v
                        then HM.insert v alt dist
                        else dist)
                    dist (filter (`HS.member` queue) neighbours)

day12 :: IO ()
day12 = do
    (graph, source, end) <- day12input
    putStrLn "⭐⭐ Day 12 ⭐⭐"
    let invg = dijkstra end (invert graph)
        p1 = invg HM.! source
        as = (HM.keys . HM.filter ((=='a') . fst)) graph
        p2 = (minimum . map (invg HM.!)) as
    putStrLn $ "Part 1: " ++ show p1
    putStrLn $ "Part 2: " ++ show p2

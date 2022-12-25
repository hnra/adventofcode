module Day24 where

import qualified Data.HashMap.Strict as HM
import Utilities (getLinesS, hm2d, bounds, unreachable, paintGrid)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.PQueue.Min (MinQueue)
import qualified Data.PQueue.Min as PQ
import GHC.List (iterate')

type Bounds = (Int, Int)
type Coord = (Int, Int)
type Blizz = (Coord, Char)
type Blizzards = [Blizz]
type Iteration = Int
type Visited = HashSet (Coord, Iteration)

day24input :: IO (Blizzards, Bounds, Coord)
day24input = do
    input <- getLinesS "24"
    let fullMap = hm2d input
        ((minX, minY), (maxX, maxY)) = bounds fullMap
        dots = HM.keys (HM.filter (=='.') fullMap)
        startX = minimum $ map fst $ filter ((==0) . snd) dots
        endX = maximum $ map fst $ filter ((==maxY) . snd) dots
        blizzards = HM.filter (`elem` ['<', '>', 'v', '^']) fullMap
    return (HM.toList blizzards, (maxX, maxY), (startX, endX))

a `mod'` b = (a - 1) `mod` (b - 1) + 1

blizz :: Bounds -> Blizzards -> [HashSet Coord]
blizz (maxX, maxY) = map (HS.fromList . map fst) . iterate' (map step)
    where
        step ((x, y), '^') = ((x, (y - 1) `mod'` maxY), '^')
        step ((x, y), 'v') = ((x, (y + 1) `mod'` maxY), 'v')
        step ((x, y), '<') = (((x - 1) `mod'` maxX, y), '<')
        step ((x, y), '>') = (((x + 1) `mod'` maxX, y), '>')
        step _ = unreachable

paint :: Bounds -> Blizzards -> IO ()
paint maxXY bs = paintGrid ((0,0), maxXY)
    (\xy -> if xy `elem` xys then (snd . head) (filter ((==xy) . fst) bs) else '.')
    where xys = map fst bs

adjacent :: Coord -> Coord -> Bounds -> Coord -> [Coord]
adjacent start end (maxX, maxY) (x, y) = filter (\xy@(x, y) ->
    xy == end ||
    xy == start ||
    x > 0 && y > 0 && x < maxX && y < maxY) xys
    where xys = [(x,y),(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

edges :: Coord -> Coord -> Bounds -> Coord -> HashSet Coord -> [Coord]
edges start end bound xy xys = filter (not . (`HS.member` xys)) (adjacent start end bound xy)

bestCase :: Coord -> Coord -> Iteration -> Iteration
bestCase (endX, endY) (x, y) i = i + abs (x - endX) + abs (y - endY)

search :: Visited -> Coord -> Coord -> Bounds -> [HashSet Coord] -> Iteration -> Coord -> Iteration
search visited start end bound bs i xy = go visited (PQ.fromList [(maxBound, (start, i))])
    where
        bc = uncurry (bestCase end)
        go :: Visited -> MinQueue (Int, (Coord, Iteration)) -> Iteration
        go visited queue
            | xy == end = i
            | otherwise = go (HS.insert (xy, i) visited) (PQ.union queue'' es')
            where
                queue' = PQ.dropWhile ((`HS.member` visited) . snd) queue
                ((_, (xy, i)), queue'') = PQ.deleteFindMin queue'
                es = zip (edges start end bound xy (bs !! (i + 1))) (repeat (i + 1))
                es' = PQ.fromList (map (\xy -> (bc xy, xy)) (filter (not . (`HS.member` visited)) es))

day24 :: IO ()
day24 = do
    (bs, bound@(maxX, maxY), (startX, endX)) <- day24input
    let start = (startX, 0)
        end = (endX, maxY)
        part1 = search HS.empty start end bound (blizz bound bs) 0 start
        i' = search HS.empty end start bound (blizz bound bs) part1 end
        part2 = search HS.empty start end bound (blizz bound bs) i' start
    putStrLn "⭐⭐ Day 24 ⭐⭐"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2

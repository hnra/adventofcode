module Day18 where

import Prelude hiding (Left, Right)
import Utilities (getLinesS, unreachable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (delete, nub)
import Data.Foldable (foldl')

type Cube = (Int, Int, Int)

data Side = Top | Bottom | Left | Right | Front | Back
    deriving (Eq, Show, Enum)

allSides = enumFrom Top

mirror Top = Bottom
mirror Bottom = Top
mirror Left = Right
mirror Right = Left
mirror Front = Back
mirror Back = Front

day18input :: IO [Cube]
day18input = map (\t -> read ("(" ++ t ++ ")")) <$> getLinesS "18"

isAdjacent :: Cube -> Cube -> Bool
isAdjacent (x, y, z) (x', y', z') = abs (x-x') + abs (y-y') + abs (z-z') == 1

adjacency :: Cube -> Cube -> Maybe Side
adjacency xyz@(x, y, z) xyz'@(x', y', z')
    | isAdjacent xyz xyz' =
        case (x-x',y-y',z-z') of
            (0, 0, 1) -> Just Top
            (0, 0, -1) -> Just Bottom
            (0, -1, 0) -> Just Left
            (0, 1, 0) -> Just Right
            (1, 0, 0) -> Just Front
            (-1, 0, 0) -> Just Back
            _ -> unreachable
    | otherwise = Nothing

type SideMap = HashMap Cube [Side]

removeAdjs :: SideMap -> [Cube] -> SideMap
removeAdjs surfaces [] = surfaces
removeAdjs surfaces (c:cs) = removeAdjs (foldr (flip removeAdj) surfaces cs) cs
    where
        removeAdj :: SideMap -> Cube -> SideMap
        removeAdj surfaces c' =
            case adjacency c c' of
                Just s -> let surfaces' = HM.adjust (delete (mirror s)) c surfaces
                          in HM.adjust (delete s) c' surfaces'
                Nothing -> surfaces

type CubeSet = HashSet Cube

bfs :: CubeSet -> [Cube] -> CubeSet -> CubeSet
bfs _ [] visited = visited
bfs lava (cube@(x, y, z):cubes) visited 
    | x > 22 || y > 22 || z > 22 || x < 0 || y < 0 || z < 0 = bfs lava cubes visited'
    | cube `HS.member` visited = bfs lava cubes visited
    | cube `HS.member` lava = bfs lava cubes visited'
    | otherwise = bfs lava (cubes ++ unvisited) visited'
    where
        neighbors = [(x-1,y,z),(x+1,y,z),(x,y-1,z),(x,y+1,z),(x,y,z-1),(x,y,z+1)]
        unvisited = filter (not . (`HS.member` visited)) neighbors
        visited' = cube `HS.insert` visited

day18 :: IO ()
day18 = do
    input <- day18input
    let
        sideMap = HM.fromList (zip input (repeat allSides))
        uncovered = removeAdjs sideMap input
        part1 = length $ concat $ HM.elems uncovered

        cells = [(x, y, z) | x <- [0 .. 21], y <- [0 .. 21], z <- [0 .. 21]]
        reachable = bfs (HS.fromList input) [(22, 22, 22)] HS.empty
        interior = filter (not . (`HS.member` reachable)) cells
        interior' = filter (not . (`elem` input)) interior

        filled = nub $ input ++ interior
        sideMap' = HM.fromList (zip filled (repeat allSides))
        uncovered' = removeAdjs sideMap' filled
        part2 = length $ concat $ HM.elems uncovered'
    putStrLn "⭐⭐ Day 18 ⭐⭐"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2

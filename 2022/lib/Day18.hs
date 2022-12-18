module Day18 where

import Prelude hiding (Left, Right)
import Utilities (getLinesS, unreachable)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (delete)

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
isAdjacent (x,y,z) (x',y',z') = abs (x-x') + abs (y-y') + abs (z-z') == 1

adjacency :: Cube -> Cube -> Maybe Side
adjacency xyz@(x,y,z) xyz'@(x',y',z')
    | isAdjacent xyz xyz' =
        case (x-x',y-y',z-z') of
            (0, 0, 1) -> Just Top
            (0, 0, -1) -> Just Bottom
            (0, 1, 0) -> Just Left
            (0, -1, 0) -> Just Right
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
                Just s -> let surfaces' = HM.adjust (delete s) c surfaces
                        in HM.adjust (delete (mirror s)) c' surfaces'
                Nothing -> surfaces

alongAxis :: Cube -> Side -> (Cube -> Bool)
alongAxis (x, y, z) Top = \(x', y', z') ->  x==x' && y==y && z<z'
alongAxis (x, y, z) Bottom = \(x', y', z') ->  x==x' && y==y && z>z'
alongAxis (x, y, z) Left = \(x', y', z') ->  x==x' && y<y' && z==z'
alongAxis (x, y, z) Right = \(x', y', z') ->  x==x' && y>y' && z==z'
alongAxis (x, y, z) Front = \(x', y', z') ->  x<x' && y==y' && z==z'
alongAxis (x, y, z) Back = \(x', y', z') ->  x>x' && y==y' && z==z'

day18 :: IO ()
day18 = do
    input <- day18input
    putStrLn "Day 18"
    let
        sideMap = HM.fromList (zip input (repeat allSides))
        uncovered = removeAdjs sideMap input
    print $ length $ concat $ HM.elems uncovered

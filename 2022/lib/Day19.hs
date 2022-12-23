module Day19 where

import Data.Attoparsec.Text hiding (take)
import Utilities (getInput)
import Data.Maybe (mapMaybe)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable(..))

data RobotType = Ore | Clay | Obs | Geo deriving (Eq, Enum)

instance Hashable RobotType where
    hashWithSalt i r = hashWithSalt i (fromEnum r)

type Cnt = HashMap RobotType Int

data Blueprint = Blueprint { _bid :: Int, _mk :: RobotType -> Cnt }

data State = State {
    _bp :: Blueprint,
    _rs :: Cnt,
    _res :: Cnt,
    _restrictions :: [RobotType]
}

type Decision = State -> State

allTypes = enumFrom Ore
fromIs i j k l = HM.fromList [(Ore, i), (Clay, j), (Obs, k), (Geo, l)]
one r = HM.fromList (map (\r' -> if r == r' then (r, 1) else (r', 0)) allTypes)
bestGeo = maximum . map ((HM.! Geo) . _res)

(-$) = HM.unionWith (-)
(+$) = HM.unionWith (+)
cnt <=$ cnt' = all (\r -> cnt HM.! r <= cnt' HM.! r) allTypes

pBlueprint :: Parser Blueprint
pBlueprint = do
    bid <- string "Blueprint " *> decimal
    ore <- string ": Each ore robot costs " *> decimal
    clay <- string " ore. Each clay robot costs " *> decimal
    obsidianOre <- string " ore. Each obsidian robot costs " *> decimal
    obsidianClay <- string " ore and " *> decimal
    geodeOre <- string " clay. Each geode robot costs " *> decimal
    geodeObs <- string " ore and " *> decimal <* " obsidian." <* endOfLine
    let
        mk Ore = fromIs ore 0 0 0
        mk Clay = fromIs clay 0 0 0
        mk Obs = fromIs obsidianOre obsidianClay 0 0
        mk Geo = fromIs geodeOre 0 geodeObs 0
    return $ Blueprint bid mk

day19input :: IO [Blueprint]
day19input = do
    input <- getInput "19"
    let (Right bs) = parseOnly (many' pBlueprint) input
    return bs

buildable :: Blueprint -> Cnt -> [RobotType]
buildable bp res = filter (\r -> _mk bp r <=$ res) allTypes

decisions :: Int -> State -> [Decision]
decisions i state =
    if length ds == length canBuild
        then ds
        else noDecision : ds
    where
        hasGeo = _rs state HM.! Geo > 0
        shouldBuild = Geo : filter (\r ->
            let robots = _rs state HM.! r
                most = maximum $ map (\r' -> _mk (_bp state) r' HM.! r) allTypes
                stock = _res state HM.! r
            in robots * i + stock < i * most) (if hasGeo then [Obs] else allTypes)
        canBuild = filter (not . (`elem` _restrictions state)) shouldBuild
        ds = mapMaybe toDeicision canBuild
        toDeicision r
            | cost <=$ _res state =
                Just $ \s ->
                    State (_bp s) (_rs s +$ one r) (_res s -$ cost) (buildable (_bp s) (_res state -$ cost))
            | otherwise = Nothing
            where cost = _mk (_bp state) r
        noDecision = \s -> State (_bp s) (_rs s) (_res s) (buildable (_bp state) (_res state))

tick :: State -> State
tick (State bp rs res restr) = State bp rs (rs +$ res) restr

genState :: Int -> State -> [State]
genState 0 state = [state]
genState t state = concatMap (genState (t-1)) ds
    where
        ds = map ($tick state) (decisions t state)

day19 :: IO ()
day19 = do
    input <- day19input
    let
        robots = one Ore
        resources = fromIs 0 0 0 0
        states = map (\bp -> State bp robots resources []) input
        part1 = sum $ zipWith (*) [1..] (map (bestGeo . genState 24) states)
        part2 = product $ map (bestGeo . genState 32) (take 3 states)
    putStrLn "⭐⭐ Day 19 ⭐⭐"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2

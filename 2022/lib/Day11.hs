module Day11 where

import Utilities (getInput, tread)
import Data.Text (Text)
import qualified Data.Text as T
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (sort)
import Data.Foldable (foldl')

data Monkey = Monkey {
    m_name :: !Int,
    m_items :: ![Int],
    m_op :: !(Int -> Int),
    m_test :: !(Int -> Int),
    m_divisor :: !Int,
    m_inspect :: !Int
}

parseMonkey :: Text -> Maybe Monkey
parseMonkey t = do
    name <- monkeyId
    items <- monkeyItems
    op <- monkeyOp
    (test, divisor) <- monkeyTest
    return (Monkey name items op test divisor 0)
    where
        (idl:isl:opl:testl:truel:falsel:_) = map T.strip (T.splitOn "\n" t)

        monkeyId :: Maybe Int
        monkeyId = do
            idstr <- T.stripPrefix "Monkey " idl
            return $ (tread . T.init) idstr

        monkeyItems :: Maybe [Int]
        monkeyItems = do
            istr <- T.stripPrefix "Starting items: " isl
            let is = map tread $ T.splitOn "," istr
            return is

        monkeyOp :: Maybe (Int -> Int)
        monkeyOp = do
            opstr <- T.stripPrefix "Operation: new = old " opl
            (op, am) <- T.uncons opstr
            case (op, am) of
               ('+', " old") -> Just $ \i -> i + i
               ('+', _) -> Just $ \i -> i + tread am
               ('*', " old") -> Just $ \i -> i * i
               ('*', _) -> Just $ \i -> i * tread am
               _ -> Nothing

        monkeyTest :: Maybe (Int -> Int, Int)
        monkeyTest = do
            divisor <- tread <$> T.stripPrefix "Test: divisible by " testl
            iftrue <- tread <$> T.stripPrefix "If true: throw to monkey " truel
            iffalse <- tread <$> T.stripPrefix "If false: throw to monkey " falsel
            return (\i -> if i `mod` divisor == 0 then iftrue else iffalse, divisor)

day11input :: IO [Monkey]
day11input = do
    monkeyBlocks <- T.splitOn "\n\n" <$> getInput "11"
    case traverse parseMonkey monkeyBlocks of
        Just monkeys -> return monkeys
        Nothing -> error "Parsing input failed"

throw :: Int -> Monkey -> Monkey
throw i (Monkey name is op test d cnt) = Monkey name (is ++ [i]) op test d cnt

turn :: (Int -> Int) -> IntMap Monkey -> Monkey -> IntMap Monkey
turn calm monkeys (Monkey n is op test d cnt) =
    IM.insert n (Monkey n [] op test d (cnt + length is)) monkeys'
    where
        is' = map (calm . op) is
        monkeys' = foldl' (\im i -> IM.adjust (throw i) (test i) im) monkeys is'

round' :: (Int -> Int) -> IntMap Monkey -> IntMap Monkey
round' calm mm =
    foldl' (\m name -> turn calm m (m IM.! name)) mm names
    where
        names = map (m_name . snd) (IM.toList mm)

toMap :: [Monkey] -> IntMap Monkey
toMap = IM.fromAscList . map (\m -> (m_name m, m))

problem :: (Int -> Int) -> Int -> [Monkey] -> Int
problem calm rounds monkeys = (product . take 2 . reverse . sort) $ map m_inspect monkeys'
    where
        mm = toMap monkeys
        monkeys' = map snd . IM.toList $ foldl' (\acc _ -> round' calm acc) mm [1..rounds]

-- | Solves day11 assuming that the input is sorted by ascending ids.
day11 :: IO ()
day11 = do
    input <- day11input
    let 
        divisor = (product . map m_divisor) input
        p1 = flip div 3
        p2 i = i `mod` divisor
    putStrLn "⭐⭐ Day 11 ⭐⭐"
    putStrLn $ "Part 1: " ++ show (problem p1 20 input)
    putStrLn $ "Part 2: " ++ show (problem p2 10000 input)

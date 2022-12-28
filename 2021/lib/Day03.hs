module Day03 where

import Text.Read (readMaybe)
import Text.Printf (printf)
import Data.Maybe (catMaybes)
import Utilities (getInputS)

newtype Binary = Binary [Bool]

instance Show Binary where
    show (Binary bs) = map (\b -> if b then '1' else '0') bs

input :: IO [Binary]
input = do
    str <- getInputS "03"
    let
        ints = fmap (Binary . catMaybes) $ (fmap . fmap) readBool (lines str)
    return ints
    where
        readChar c = readMaybe (c:"")
        readBool c = do
            i <- readChar c
            case i of
                0 -> Just False
                1 -> Just True
                _ -> Nothing

bits :: Binary -> Int
bits (Binary bs) = length bs

toInts :: Binary -> [Integer]
toInts (Binary bs) = fmap (\b -> if b then 1 else -1) bs

toBin :: [Integer] -> Binary
toBin xs = Binary $ fmap (\x -> x >= 0) xs

inverse :: Binary -> Binary
inverse (Binary bs) = Binary (fmap not bs)

mostCommonBits :: [Binary] -> Binary
mostCommonBits bs = toBin $ foldl go start bs
    where
        start = take (bits (bs !! 0)) $ repeat 0
        go score b = zipWith (+) score (toInts b)

toDec :: Binary -> Integer
toDec (Binary bs) =
    (sum . zipWith (\b i -> if b then 2^i else 0) bs) exponents
    where
        len = length bs
        exponents = [len - 1, (len - 2)..0]

p1 :: [Binary] -> Integer
p1 b = (toDec mcb) * (toDec . inverse) mcb
    where
        mcb = mostCommonBits b

(!?) :: Binary -> Int -> Maybe Bool
(Binary bs) !? i
    | i < length bs = Just $ bs !! i
    | otherwise = Nothing

some :: Binary -> Binary -> Bool
some (Binary bs1) (Binary bs2) =
    any id $ zipWith (&&) bs1 bs2

filterBit :: Int -> (Bool -> Bool) -> [Binary] -> Maybe [Binary]
filterBit i f b = do
    mcb <- (mostCommonBits b) !? i
    let blen = bits (b !! 0)
    let fbin = Binary ([x == i | x <- [0..blen]])
    let filt = some fbin . (if f mcb then id else inverse)
    return $ filter filt b

p2 :: [Binary] -> Maybe Integer
p2 inp = do
    oxy <- go id 0 inp
    scrub <- go not 0 inp
    return $ toDec oxy * toDec scrub
    where
        invInp = fmap inverse inp
        go f i inp =
            case filterBit i f inp of
                Just (x:[]) -> Just x
                Just xs -> go f (i+1) xs
                Nothing -> Nothing

day3 = do
    bs <- input
    putStrLn "⭐⭐ Day 3 ⭐⭐"
    printf "Part 1: %i\n" (p1 bs)
    case p2 bs of
        Just p2 -> printf "Part 2: %i\n" p2
        Nothing -> print "Failed part 2"


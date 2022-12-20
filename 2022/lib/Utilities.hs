module Utilities (
    tread, getInput, getInputS, getLines, getLinesS, arr2d, hm2d, eithersToList, unreachable
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified  Data.Text.IO as TIO
import GHC.Arr
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Vector (Vector)
import qualified Data.Vector as V

tread :: Read a => Text -> a
tread = read . T.unpack

getInput :: String -> IO Text
getInput i = TIO.readFile ("inputs/day" ++ i)

getLines :: String -> IO [Text]
getLines path = T.lines <$> getInput path

getInputS :: String -> IO String
getInputS i = readFile ("inputs/day" ++ i)

getLinesS :: String -> IO [String]
getLinesS path = lines <$> getInputS path

arr2d :: Int -> Int -> a -> Array (Int, Int) a
arr2d width height a =
    listArray ((0, 0), (width - 1, height - 1)) (replicate (width * height) a)

merge :: (a, [(b, c)]) -> [((a, b), c)]
merge (a, list) = map (\(b, c) -> ((a, b), c)) list

hm2d :: [[a]] -> HashMap (Int, Int) a
hm2d = HM.fromList . concatMap merge . zip [0..] . map (zip [0..])

eithersToList :: (a -> c) -> (b -> c) -> [Either a b] -> [c]
eithersToList ac bc = map (abc ac bc)
    where
        abc :: (a -> c) -> (b -> c) -> Either a b -> c
        abc ac _ (Left a) = ac a
        abc _ bc (Right b) = bc b

unreachable :: a
unreachable = error "Oh boy, I'm in trouble!"

floyd :: Eq a => [a] -> Maybe (Int, Int)
floyd fs' = do
    let
        fs = V.fromList fs'
        go tortoise hare = do
            t <- fs V.!? tortoise
            h <- fs V.!? hare
            if h == t
                then return (tortoise, hare)
                else go (tortoise + 1) (hare + 2)
    (tortoise, hare) <- go 1 2
    
    let
        go' mu tortoise hare = do
            t <- fs V.!? tortoise
            h <- fs V.!? hare
            if t == h
                then return (mu, tortoise)
                else go' (mu + 1) (tortoise + 1) (hare + 1)
    (mu, tortoise') <- go' 0 0 hare

    let
        go'' lam hare = do
            t <- fs V.!? tortoise'
            h <- fs V.!? hare
            if t == h
                then return (lam, hare)
                else go'' (lam + 1) (hare + 1)
    (lam, hare') <- go'' 1 (tortoise' + 1)

    return (lam, mu)

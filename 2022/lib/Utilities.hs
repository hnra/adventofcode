module Utilities (
    tread, getInput, getInputS, getLines, getLinesS, arr2d, hm2d, eithersToList, unreachable
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified  Data.Text.IO as TIO
import GHC.Arr
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

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

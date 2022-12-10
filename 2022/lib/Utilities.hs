module Utilities (
    tread, getInput, getLines, arr2d
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified  Data.Text.IO as TIO
import GHC.Arr

tread :: Read a => Text -> a
tread = read . T.unpack

getInput :: String -> IO Text
getInput i = TIO.readFile ("inputs/day" ++ i)

getLines :: String -> IO [Text]
getLines path = T.lines <$> getInput path

arr2d :: Int -> Int -> a -> Array (Int, Int) a
arr2d width height a = array ((0, 0), (width - 1, height - 1)) arr
    where
        row = zip [0..] (replicate width a)
        rows = zip [0..] (replicate height row)
        arr = concatMap (\(y, xs) -> map (\(x, c) -> ((x, y), c)) xs) rows

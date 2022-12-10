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
arr2d width height a =
    listArray ((0, 0), (width - 1, height - 1)) (replicate (width * height) a)

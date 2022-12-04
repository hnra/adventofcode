module Utilities (
    tread, getInput, getLines
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified  Data.Text.IO as TIO

tread :: Read a => Text -> a
tread = read . T.unpack

getInput :: String -> IO Text
getInput i = TIO.readFile ("inputs/day" ++ i)

getLines :: String -> IO [Text]
getLines path = T.lines <$> getInput path

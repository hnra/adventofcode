module Day20 where

import Utilities (getLinesS)
import Data.Sequence (Seq ((:<|)))
import qualified Data.Sequence as S
import Control.Monad (replicateM)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe (fromJust)

type Input = Seq Int
type IdMap = IntMap Int

day20input :: IO [Int]
day20input = map read <$> getLinesS "20"

decrypt :: IdMap -> Input -> Input -> Input
decrypt idmap order seq = go seq order
    where
        go seq (uuid:<|ss) = go seq'' ss
            where
                ix = fromJust (S.findIndexL (==uuid) seq)
                curr = idmap IM.! uuid
                seq' = ix `S.deleteAt` seq
                dest = (ix - 1 + curr) `mod` S.length seq' + 1
                seq'' = S.insertAt dest uuid seq'
        go seq _ = seq

grove :: IdMap -> Input -> Int
grove idmap seq =
    (sum . map ((idmap IM.!) . (seq `wrapIx`))) ixs
    where
        zeroUuid = (head . IM.keys . IM.filter (==0)) idmap
        (Just zeroIx) = S.findIndexL (==zeroUuid) seq
        ixs = map (+zeroIx) [1000, 2000, 3000]
        wrapIx seq ix = seq `S.index` (ix `mod` S.length seq)

day20 :: IO ()
day20 = do
    input <- day20input
    let
        ids = [0..length input - 1]
        idmap = IM.fromList (zip ids input)
        seq = S.fromList ids
        part1 = grove idmap (decrypt idmap seq seq)
        idmap' = IM.map (*811589153) idmap
        part2 = grove idmap' (iterate (decrypt idmap' seq) seq !! 10)
    putStrLn "⭐⭐ Day 20 ⭐⭐"
    putStrLn $ "Part 1: " ++ show part1
    putStrLn $ "Part 2: " ++ show part2

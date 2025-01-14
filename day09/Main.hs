module Main where

import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Grid (Coord, Grid)
import qualified Grid as G
import Utils (readDigits)

type Cell = (Coord, Int)

isLow :: Grid Int -> Cell -> Bool
isLow grid (coord, digit) = all (digit<)
                          . map snd
                          $ G.vonNeum grid coord

riskLevel :: Cell -> Int
riskLevel = (+1) . snd

------------
-- Part 2 --
------------

basinSize :: Grid Int -> Cell -> Int
basinSize grid cell = go Set.empty
                         (Seq.singleton cell)
                         (Set.delete cell . Set.fromList . G.enumerate $ grid)
  where
    go accum Seq.Empty _                          = Set.size accum
    go accum (q@(coord, height):<|qs)  unvisiteds = go accum' queue' unvisiteds'
      where
            nexts       = Seq.fromList
                        . filter ((>height) . snd)
                        . filter ((/=9) . snd)
                        . filter (`Set.member` unvisiteds) 
                        $ G.vonNeum grid coord
            accum'      = Set.insert q accum
            queue'      = qs Seq.>< nexts
            unvisiteds' = Set.delete q unvisiteds

main :: IO ()
main = do
    grid    <- G.fromList . map readDigits . lines <$> readFile "input.txt"
    let lows = filter (isLow grid) $ G.enumerate grid

    -- Part 1
    print . sum . map riskLevel $ lows

    -- Part 2
    print . product
          . take 3
          . reverse
          . sort
          . map (basinSize grid)
          $ lows

    print $ "Done."
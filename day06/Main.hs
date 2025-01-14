module Main where

import qualified Data.Map as Map
import Utils (ordCounts, readInt, tok)

parseInput :: String -> [Int]
parseInput line = map (\ i -> Map.findWithDefault 0 i counts) [0 .. 8]
  where
    counts = ordCounts . map readInt . tok "," $ line

-- The problem can be described succinctly using a Leslie matrix model.
-- We get an iterated map with a coefficient matrix of A=
-- [0,1,0,0,0,0,0,0,0]
-- [0,0,1,0,0,0,0,0,0]
-- [0,0,0,1,0,0,0,0,0]
-- [0,0,0,0,1,0,0,0,0]
-- [0,0,0,0,0,1,0,0,0]
-- [0,0,0,0,0,0,1,0,0]
-- [1,0,0,0,0,0,0,1,0]
-- [0,0,0,0,0,0,0,0,1]
-- [1,0,0,0,0,0,0,0,0].
-- Thus, for one time step we have n_(t+1) = A . n_t, where n is in R^9.
-- For t time steps starting at n_0, we would get n_t = exp(A*t) . n_0,
-- however the matrix exponential yields some gnarly expressions,
-- if we want a general solution. Hence, we'll just do repeated
-- multiplication and call it a day.

-- One step, i.e. one matrix multiplication. We expand all
-- equations, so that we can use native data types.
step :: [Int] -> [Int]
step ns = [n0', n1', n2', n3', n4', n5', n6', n7', n8']
  where
    n0  = ns !! 0
    n1  = ns !! 1
    n2  = ns !! 2
    n3  = ns !! 3
    n4  = ns !! 4
    n5  = ns !! 5
    n6  = ns !! 6
    n7  = ns !! 7
    n8  = ns !! 8
    n0' = n1
    n1' = n2
    n2' = n3
    n3' = n4
    n4' = n5
    n5' = n6
    n6' = n7 + n0
    n7' = n8
    n8' = n0


main :: IO ()
main = do
    input <- parseInput . head . lines <$> readFile "input.txt"

    print . sum . (!!80) . iterate step $ input

    print . sum . (!!256) . iterate step $ input

    print $ "Done."

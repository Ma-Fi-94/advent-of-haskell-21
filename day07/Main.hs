module Main where

import Data.Function (on)
import Data.List (minimumBy, sort)
import Utils (readInt, tok)

parseInput :: String -> [Int]
parseInput = map readInt . tok ","

------------
-- Part 1 --
------------

-- ANY median minimises the l1 norm, hence we
-- just return the first one in case of an
-- even number of elements.
firstMedian :: [Int] -> Int
firstMedian xs = (sort xs) !! (n `div` 2)
  where
    n = length xs

-- Given a list of numbers and a number,
-- calculate the sum of absolute deviations.
sumAbsDev :: [Int] -> Int -> Int
sumAbsDev xs x = sum . map abs . map (+(-x)) $ xs

------------
-- Part 2 --
------------

-- The arithmetic mean minimises the l2 norm, but we
-- care about some quadratic-linear thingy due to
-- triangle numbers. One may show that the optimum
-- is guaranteed to be within +/- 1 unit of the mean.
intMean :: [Int] -> Int
intMean xs = sum xs `div` length xs

-- The new metric, based on triangle numbers.
triDist x y = (d * (d + 1)) `div` 2
  where d = abs (x - y)

-- Sum of triangle distances for a list of Ints.
sumTri :: [Int] -> Int -> Int
sumTri xs x = sum . map (triDist x) $ xs

main :: IO ()
main = do
    input <- parseInput . head . lines <$> readFile "test.txt"

    print $ sumAbsDev input (firstMedian input)

    print . minimum
          . map (\ h -> sumTri input h)
          . (\ h -> [h - 1, h, h + 1])
          $ intMean input


    print $ "Done."

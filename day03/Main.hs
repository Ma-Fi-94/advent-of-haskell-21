{-# LANGUAGE LambdaCase #-}

module Main where

import Utils (readDigits)

import Debug.Trace (trace)
-----------------------------------------------
-- A little binary type with some operations --
-----------------------------------------------

type Binary = [Int]

-- Bitwise addition
bSum :: Binary -> Binary -> Binary
bSum b1 b2 = zipWith (+) b1 b2

-- Bitwise negation
bNot :: Binary -> Binary
bNot = map (\case {0 -> 1; 1 -> 0})

-- Conversion to decimal
bin2dec :: Binary -> Int
bin2dec = sum . zipWith (*) [2 ^ i | i <- [0..]] . reverse

-- Find the most common digit at every position
commons :: [Binary] -> Binary
commons bs = map (\x -> if x > (length bs `div` 2) then 1 else 0)
           $ foldl1 bSum bs

-- Find the most uncommon digit at every position
uncommons :: [Binary] -> Binary
uncommons = bNot . commons

--------------------------
-- The actual solutions --
--------------------------

-- Part 1 is just the definition in the task.
part1 :: [Binary] -> Int
part1 bs = bin2dec (commons bs) * bin2dec (uncommons bs)

-- Part 2 requires two helpers for filtering.
part2 :: [Binary] -> Int
part2 bs = bin2dec (filterCommons bs) * bin2dec (filterUncommons bs)

-- Position-wise filtering for the most common digit.
filterCommons :: [Binary] -> Binary
filterCommons = go 0
  where
    go i bs
        | length bs == 1        = head bs
        | otherwise             = go (i + 1) bs'
      where
        bs'    = filter ((==common) . (!! i)) bs
        count0 = length . filter (==0) . map (!! i) $ bs
        count1 = length . filter (==1) . map (!! i) $ bs
        common = if   count1 >= count0
                 then 1
                 else 0

-- Position-wise filtering for the most UNcommon digit.
filterUncommons :: [Binary] -> Binary
filterUncommons = go 0
  where
    go i bs
        | length bs == 1        = head bs
        | otherwise             = go (i + 1) bs'
      where
        bs'      = filter ((==uncommon) . (!! i)) bs
        count0   = length . filter (==0) . map (!! i) $ bs
        count1   = length . filter (==1) . map (!! i) $ bs
        uncommon = if   count1 < count0
                   then 1
                   else 0

main :: IO ()
main = do
    numbers <- map readDigits . lines <$> readFile "input.txt"

    print $ part1 numbers

    print $ part2 numbers

    print $ "Done."

{-# LANGUAGE LambdaCase #-} 

module Main where

import Data.List (sort)
import Data.Either (lefts, rights, fromLeft, fromRight)

-- A line is either corrupted, or incomplete.
-- If it's corrupted, we return its first
-- wrong character. If it's incomplete, we
-- return what's left on the stack.

type Corrupted  = Char
type Incomplete = String
type Result     = Either Corrupted Incomplete

-- Analyse a line using a stack.
analyse :: String -> Result
analyse = go []
  where
    go stack  []            = Right stack
    go stack  string@(x:xs)
        | x `elem` "([{<"               = go (x : stack) xs
        | x == ')' && head stack == '(' = go (tail stack) xs
        | x == ']' && head stack == '[' = go (tail stack) xs
        | x == '}' && head stack == '{' = go (tail stack) xs
        | x == '>' && head stack == '<' = go (tail stack) xs
        | otherwise                     = Left x

-- Score a wrong character
scoreChar :: Char -> Int
scoreChar = \case
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137

------------
-- Part 2 --
------------

-- Close an incomplete line based on what's left on the stack.
close :: String -> String
xclose []     = []
close (x:xs) = x' : close xs
  where x' = case x of {'(' -> ')'; '[' -> ']'; '{' -> '}'; '<' -> '>'}
        

-- Score an incomplete line
scoreIncomplete :: String -> Int
scoreIncomplete stack = go 0 (close stack)
  where
    go i []     = i
    go i (x:xs) = go i' xs
      where i' = i * 5 + case x of {')' -> 1; ']' -> 2; '}' -> 3; '>' -> 4}


main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"
    let results = map analyse inputLines

    -- Part 1
    print . sum
          . map scoreChar
          . lefts
          $ results

    -- Part 2
    print . ( \ list -> list !! (length list `div` 2))
          . sort
          . map scoreIncomplete
          . rights
          $ results



    print $ "Done."


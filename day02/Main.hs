{-# LANGUAGE LambdaCase #-}

module Main where

import Utils (readInt, tok)

data Command = F Int | D Int | U Int deriving (Show, Eq)
type State   = (Int, Int)        -- (h, d)
type State'  = (Int, Int, Int)   -- (h, d, a)

parseLine :: String -> Command
parseLine line = case (tok " " line) of
    ["up", rest]      -> (U (readInt rest))
    ["down", rest]    -> (D (readInt rest))
    ["forward", rest] -> (F (readInt rest))

step :: State -> Command -> State
step (h, d) = \case
    (U i) -> (h, d - i)
    (D i) -> (h, d + i)
    (F i) -> (h + i, d)

step' :: State' -> Command -> State'
step' (h, d, a) = \case
    (U i) -> (h, d, a - i)
    (D i) -> (h, d, a + i)
    (F i) -> (h + i, d + a * i, a)

main :: IO ()
main = do
    commands <- map parseLine . lines <$> readFile "input.txt"

    print . uncurry (*) . foldl step (0, 0) $ commands

    print . (\ (h, d, _) -> h * d) . foldl step' (0, 0, 0) $ commands

    print $ "Done."

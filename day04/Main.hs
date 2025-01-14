module Main where

import Data.List (transpose)
import Utils (map2, readInt, tok)

-- We model a Board as a list of lists of tuples (number, marked).
type Board = [[(Int, Bool)]]

-- Does exactly what it says on the tin.
parseBoard :: [String] -> Board
parseBoard = map2 ((, False) . readInt)
           . map (tok " ")

-- Given a board and a called number, mark the number on the board.
markNumber :: Int -> Board -> Board
markNumber i board = map2 mark board
  where
    mark (number, called) = if   number == i
                            then (number, True)
                            else (number, called)

-- Check whether a board has won, i.e. has a complete column or row.
isWinner :: Board -> Bool
isWinner board = or (map and calleds) || or (map and calledsT)
  where
    calleds  = map2 snd board
    calledsT = transpose calleds

-- The sum of all unmarked numbers of a board, used for scoring.
sumUnmarked :: Board -> Int
sumUnmarked = sum . map fst . filter (not . snd) . concat

-- And the scoring, as defined in the task.
score :: Board -> Int -> Int
score board number = (sumUnmarked board) * number

-- Mark called numbers until the first board wins, return its score,
-- assuming it is unique. If it was not unique, we would process
-- the head of the list of all winners.
part1 :: [Board] -> [Int] -> Int
part1 = go
  where
    go boards (n:ns)
        | any isWinner boards' = score (head . filter isWinner $ boards') n
        | otherwise            = go boards' ns
      where
        boards' = map (markNumber n) boards

-- Like part1, but instead we search for the board that wins last.
-- To this end, we remove all winning boards on the way, until
-- only one board remains, and wait for this one to win.
part2 :: [Board] -> [Int] -> Int
part2 = go
  where
    go boards (n:ns)
        | length boards' == 1 && isWinner (head boards') = score (head boards') n
        | otherwise                                      = go (filter (not . isWinner) boards') ns
      where
        boards' = map (markNumber n) boards


main :: IO ()
main = do
    blocks     <- tok [""] . lines <$> readFile "input.txt"
    let numbers = map readInt . tok "," . head . head $ blocks
    let boards  = map parseBoard . tail $ blocks

    print $ part1 boards numbers

    print $ part2 boards numbers

    print $ "Done."

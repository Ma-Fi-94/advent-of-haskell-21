module Main where

import qualified Data.Map as Map
import Utils (intRange, map2, readInt, tok)

-- Straightforward typing. Note that Point satisfied Ord by default.
type X     = Int
type Y     = Int
type Point = (X, Y)
type Line  = (Point, Point)

-- Does exactly what it says on the tin.
parseLine :: String -> Line
parseLine line = ((nums !! 0, nums !! 1), (nums !! 2, nums !! 3))
  where
    nums = map readInt . tok " ,->" $ line

-- Given a line, return a list of all points it touches,
-- if it's an orthogonal line.
expandOrtho :: Line -> [Point]
expandOrtho ((x1, y1), (x2, y2))
    | x1 == x2  = [(x1, y) | y <- intRange y1 y2]
    | y1 == y2  = [(x, y1) | x <- intRange x1 x2]
    | otherwise = []

-- And the same but for all lines including diagonal ones.
-- Importantly, diagonal lines are guaranteed to have a
-- 45 degree angle, which simplifies the task a lot.
expandAll :: Line -> [Point]
expandAll ((x1, y1), (x2, y2))
    | x1 == x2  = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
    | y1 == y2  = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
    | otherwise = zip (intRange x1 x2) (intRange y1 y2)

main :: IO ()
main = do
    inputLines <- map parseLine . lines <$> readFile "input.txt"

    -- Part 1
    print . Map.size
          . Map.filter (>1)
          . Map.unionsWith (+)
          . map Map.fromList
          . map2 (,1)            -- Instead, we merge a list of Maps with (+).
          . map expandOrtho      -- We willingly don't concatMap here for speed.
          $ inputLines

    -- Part 2
    print . Map.size
          . Map.filter (>1)
          . Map.unionsWith (+)
          . map Map.fromList
          . map2 (,1)
          . map expandAll
          $ inputLines

    print $ "Done."

module Main where

import Data.Set (Set)
import qualified Data.Set as Set
import Utils (dropNondigits, readInt, tok, tuplify2)

type Coord  = (Int, Int)
data Fold   = X Int | Y Int deriving (Eq, Show)

parseInput :: String -> (Set Coord, [Fold])
parseInput input = (coords, folds)
  where
    coords      = Set.fromList . map parseCoord $ (blocks !! 0)
    folds       = map parseFold $ (blocks !! 1)
    blocks      = tok [""] . lines $ input
    parseCoord  = tuplify2 . map readInt . tok ","
    parseFold str
        | 'x' `elem` str = X i
        | 'y' `elem` str = Y i
      where
        i = readInt $ dropNondigits str

doFold :: Set Coord -> Fold -> Set Coord
doFold coords fold = case fold of
    (X x0) -> Set.map (\ (x, y) -> if   x <= x0
                                   then (x, y)
                                   else (2 * x0 - x, y))
              coords
    (Y y0) -> Set.map (\ (x, y) -> if   y <= y0
                                   then (x, y)
                                   else (x, 2 * y0 - y))
              coords

run :: [Fold] -> Set Coord -> Set Coord
run folds coords = foldl doFold coords folds

render :: Set Coord -> [String]
render coords = lines $ go 0 0
  where
    go x y
        | y == ymax + 1 = ""
        | x == xmax + 1 = '\n' : go 0 (y + 1)
        | otherwise     = if (x, y) `Set.member` coords
                          then '#' : go (x + 1) y
                          else ' ' : go (x + 1) y
                          
    xmax = maximum . map fst $ Set.elems coords
    ymax = maximum . map snd $ Set.elems coords

main :: IO ()
main = do
    (coords, folds) <- parseInput <$> readFile "input.txt"

    print . Set.size . run (take 1 folds) $ coords

    mapM_ print . render $ run folds coords

    print $ "Done."


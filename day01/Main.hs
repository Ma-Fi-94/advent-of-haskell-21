module Main where

import Utils (readInt)

compareWindows :: (Num a, Ord a) => Int -> [a] -> Int
compareWindows i xs = length
                    . filter (>0)
                    $ zipWith (-) (drop i xs) xs

main :: IO ()
main = do
    xs <- map readInt . lines <$> readFile "input.txt"

    print $ compareWindows 1 xs
    
    print $ compareWindows 3 xs

    print $ "Done."


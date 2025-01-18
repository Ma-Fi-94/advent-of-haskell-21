module Main where

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Utils (ngrams, ordCounts, tok)

-- We only need to consider bigrams of the input string,
-- because all trasformations are applied in parallel.
type Ngram  = String
type Counts = Map Ngram Int
type Rules  = Map Ngram Char

-- Does exactly what it says on the tin.
parseInput :: String -> (Counts, Rules)
parseInput input = (counts, rules)
  where
    blocks = tok [""] . lines $ input
    counts = ordCounts . ngrams 2 . head $ blocks !! 0    
    rules  = Map.fromList . map (\ s -> (take 2 s, last s)) $ blocks !! 1

-- Given the ruleset and a bigram with a given count,
-- return a Map containing the two new bigrams and
-- their counts (identical to input count)
insert :: Rules -> (Ngram, Int) -> Counts
insert rules (ngram, count) = case ngram `Map.lookup` rules of
    Nothing     -> Map.singleton ngram count
    (Just char) -> Map.fromList [((ngram !! 0):[char], count),
                                  (char:[ngram !! 1],  count)]

-- One step: Map `insert` over all elements of the Counts Map.
step :: Rules -> Counts -> Counts
step rules counts = Map.unionsWith (+)
                  . map (insert rules)
                  $ Map.assocs counts

-- Find the difference in counts of the most and least frequent "element".
-- For calculating the counts, it suffices to just consider the second
-- "element" of every bigram, but we need to add 1 to the first "element"
-- of the input string, which is why we need to pass the Template too.
delta :: String -> Counts -> Int
delta template counts = (\ freqs -> maximum freqs - minimum freqs)
                      . sort
                      . Map.elems
                      . Map.unionsWith (+)
                      . ((Map.singleton (head template) 1):)
                      . map (\ (bigram, count) -> Map.singleton (bigram !! 1) count)
                      $ Map.assocs counts


main :: IO ()
main = do
    fileContents       <- readFile "input.txt"
    let template        = head . lines $ fileContents
    let (counts, rules) = parseInput fileContents

    -- Part 1
    print . delta template . (!!10) . iterate (step rules) $ counts

    -- Part 2
    print . delta template . (!!40) . iterate (step rules) $ counts

    print $ "Done."


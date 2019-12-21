module Day4 (day04a, day04b) where

import           Data.List.Split                ( splitOn )
import           Data.List               ( group )
import Data.Char (digitToInt)

-- low  = [3,5,7,2,5,3]
-- high = [8,9,2,9,4,2]

-- minTwoAdjacent xs = (length (group xs)) <= 5

atleastTwoAdjacent xs = length (filter (>= 2) (map length (group xs))) >= 1

maxTwoInAdjacent xs = length (filter (== 2) (map length (group xs))) >= 1

fromDigits :: [Int] -> Int
fromDigits = read . concatMap show

solveA [low, high] = [ [d0, d1, d2, d3, d4, d5]
                     | d0 <- [d0L..d0H]
                     , d1 <- [d0..9]
                     , d2 <- [d1..9]
                     , d3 <- [d2..9]
                     , d4 <- [d3..9]
                     , d5 <- [d4..9]
                     , atleastTwoAdjacent [d0, d1, d2, d3, d4, d5]
                     , fromDigits [d0, d1, d2, d3, d4, d5] <= 892942
                     , fromDigits [d0, d1, d2, d3, d4, d5] >= 357253
                     ]
  where [(d0L, d0H), (d1L, d1H), (d2L, d2H), (d3L, d3H), (d4L, d4H), (d5L, d5H)] = zip (map digitToInt low) (map digitToInt high)

day04a = show . length . solveA . head . fmap (splitOn ",") . lines

solveB [low, high] = [ [d0, d1, d2, d3, d4, d5]
                     | d0 <- [d0L..d0H]
                     , d1 <- [d0..9]
                     , d2 <- [d1..9]
                     , d3 <- [d2..9]
                     , d4 <- [d3..9]
                     , d5 <- [d4..9]
                     , atleastTwoAdjacent [d0, d1, d2, d3, d4, d5]
                     , maxTwoInAdjacent [d0, d1, d2, d3, d4, d5]
                     , fromDigits [d0, d1, d2, d3, d4, d5] <= 892942
                     , fromDigits [d0, d1, d2, d3, d4, d5] >= 357253
                     ]
  where [(d0L, d0H), (d1L, d1H), (d2L, d2H), (d3L, d3H), (d4L, d4H), (d5L, d5H)] = zip (map digitToInt low) (map digitToInt high)

day04b :: String -> String
day04b = show . length . solveB . head . fmap (splitOn ",") . lines

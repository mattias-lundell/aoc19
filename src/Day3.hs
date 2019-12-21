module Day3 (day03a, day03b) where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.List                      ( scanl' )
import qualified Data.List as List
  
import           Data.List.Split                ( splitOn )
import           Util                           ( manhaDist )

type Coordinate = (Int, Int)

type Step = (Direction, Int)
type Direction = Char

origin :: Coordinate
origin = (0, 0)


parseStep :: String -> Step
parseStep (d:l) = (d, read l)

parseSteps :: [String] -> [Step]
parseSteps = map parseStep

takeStep :: Coordinate -> Int -> Step -> [(Coordinate, Int)]
takeStep (x, y) d0 ('U', d) = [((x + offset, y), d0 + offset) | offset <- [0..d]]
takeStep (x, y) d0 ('L', d) = [((x, y - offset), d0 + offset) | offset <- [0..d]]
takeStep (x, y) d0 ('D', d) = [((x - offset, y), d0 + offset) | offset <- [0..d]]
takeStep (x, y) d0 ('R', d) = [((x, y + offset), d0 + offset) | offset <- [0..d]]

makeWire :: [String] -> Map Coordinate Int
makeWire s = M.fromList $ makeWire' (origin, 0) [] (parseSteps s)
  where
    makeWire' :: (Coordinate, Int) -> [(Coordinate, Int)] -> [Step] -> [(Coordinate, Int)]
    makeWire' (c0, d) cs (s:ss) = let next = takeStep c0 d s in makeWire' (List.last next) (next ++ cs) ss
    makeWire' _ cs [] = cs

solveA :: [[String]] -> Int
solveA (a:b:[]) = minimum $ map (manhaDist origin . fst) (M.toList intersections)
  where intersections = (M.delete origin (M.intersection (makeWire a) (makeWire b)))


solveB :: [[String]] -> Int
solveB (a:b:[]) = minimum (map snd ( M.toList( M.unionWith (+) intersectionsA intersectionsB)))
  where intersectionsA = M.delete origin (M.intersection (makeWire a) (makeWire b) )
        intersectionsB = M.delete origin (M.intersection (makeWire b) (makeWire a) )

day03a :: String -> String
day03a = show . solveA . fmap (splitOn ",") . lines


day03b :: String -> String
day03b = show . solveB . fmap (splitOn ",") . lines

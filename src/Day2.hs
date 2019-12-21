module Day2 (day02a, day02b) where

import           Data.List.Split                ( splitOn )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import Debug.Trace (trace)

type Mem = Vector Int
type Ip = Int

run :: Mem -> Mem
run mem = step mem 0

runReplace :: Int -> Int -> Mem -> Int
runReplace noun verb mem = V.head $ run (mem V.// [(1, noun), (2, verb)])

step :: Mem -> Ip -> Mem
step mem ip
  | op == 1 = step (add mem ip) (ip+4)
  | op == 2 = step (mul mem ip) (ip+4)
  | op == 99 =  mem
  where
    op = getAddr mem ip 0

getAddr :: Vector Int -> Int -> Int -> Int
getAddr mem ip offset = mem V.! (ip + offset)

readAddr :: Vector Int -> Int -> Int -> Int
readAddr mem ip offset = mem V.! (getAddr mem ip offset)

add :: Mem -> Ip -> Mem
add mem ip = mem V.// [(getAddr mem ip 3, (readAddr mem ip 1) + (readAddr mem ip 2))]

mul :: Mem -> Ip -> Mem
mul mem ip = mem V.// [(getAddr mem ip 3, (readAddr mem ip 1) * (readAddr mem ip 2))]

day02a :: String -> String
day02a = show . (runReplace 12 2) . V.fromList . fmap read . splitOn ","

day02b :: String -> String
day02b = show . f . V.fromList . fmap read . splitOn ","

f :: Mem -> Int
f mem = head [ 100 * noun + verb | noun <- [0..99], verb <- [0..99], runReplace noun verb mem == 19690720 ]

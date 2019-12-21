module Day6 (day06a, day06b, toDigits) where

import qualified Data.List as L
import           Data.List.Split                ( splitOn )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as M
import Debug.Trace (trace)
import           Control.Monad.State.Lazy
import           Data.Maybe                     ( fromMaybe )


type CPUState = State CPU

data CPU = CPU { mem :: IntMap Int
               , ip :: Int
               , inputs :: [Int]
               , outputs :: [Int]
               , rb :: Int
               } deriving (Eq, Show)

newtype Pointer = Pointer { getPtr :: Int } deriving (Eq, Show)

data Param = Ptr Int | Val Int deriving (Eq, Show)

data Op = ADD Param Param Param
        | MUL Param Param Param
        | INPUT Param
        | OUTPUT Param
        | JNZ Param Param
        | JZ Param Param
        | CMPLT Param Param Param
        | CMPEQ Param Param Param
        | RB Param
        | HALT
        deriving (Eq, Show)

eval :: Op -> CPUState ()
eval (ADD x y r) = do
  x' <- deref x
  y' <- deref y
  set r (x' + y')
  incIp 4
eval (MUL x y r) = do
  x' <- deref x
  y' <- deref y
  set r (x' * y')
  incIp 4
eval (INPUT r) = do
  i' <- getInput
  set r i'
  incIp 2
eval (OUTPUT x) = do
  x' <- deref x
  output x'
  incIp 2
eval (JNZ x y) = do
  x' <- deref x
  y' <- deref y
  if x' /= 0 then setIp y' else incIp 3
eval (JZ x y) = do
  x' <- deref x
  y' <- deref y
  if x' == 0 then setIp y' else incIp 3
eval (CMPLT x y r) = do
  x' <- deref x
  y' <- deref y
  if x' < y' then set r 1 else set r 0
  incIp 4
eval (CMPEQ x y r) = do
  x' <- deref x
  y' <- deref y
  if x' == y' then set r 1 else set r 0
  incIp 4
eval (RB x) = do
  x' <- deref x
  setRb x'
  incIp 2
eval HALT = pure ()

incIp :: Int -> CPUState ()
incIp i = modify $ \s -> s { ip = ip s + i }

setIp :: Int -> CPUState ()
setIp i = modify $ \s -> s { ip = i }
  
setRb :: Int -> CPUState ()
setRb i = modify $ \s -> s { rb = rb s + i }
  
getInput :: CPUState Int
getInput = do
  s <- get 
  let input = head (inputs s)
  modify $ \s -> s {inputs = tail (inputs s)}
  return input 

output :: Int -> CPUState ()
output i = modify $ \s -> s { outputs = i : outputs s }

set :: Param -> Int -> CPUState ()
set (Ptr p) val = modify $ \s -> s { mem = M.insert p val (mem s) }

deref :: Param -> CPUState Int
deref (Ptr p) =
  fromMaybe (error ("out of bounds deref of pointer: " ++ show p))
  . (M.!? p)
  . mem
  <$> get
deref (Val v) = return v

next :: CPU -> Op
-- next cpu = let op = next' cpu in trace (show op) op
next = next'
next' cpu = case op of
             [1, 0] -> ADD x y z
             [2, 0] -> MUL x y z
             [3, 0] -> INPUT x
             [4, 0] -> OUTPUT x
             [5, 0] -> JNZ x y
             [6, 0] -> JZ x y
             [7, 0] -> CMPLT x y z
             [8, 0] -> CMPEQ x y z
             [9, 0] -> RB x
             [9, 9] -> HALT
             x -> error $ "undefined op: " <> show cpu <> " " <> show op <> " " <> show digits
  where instruction = fromMaybe (error ("out of bounds deref of pointer: " ++ show i)) (m M.!? i)
        op = take 2 digits
        digits = take 5 $ (reverse (toDigits instruction) <> repeat 0)
        x = parseParam 0
        y = parseParam 1
        z = parseParam 2
        i = ip cpu
        m = mem cpu
        parseParam n = let addr = succ i + n in fromMaybe (error ("out of bounds " <> show n <> " " <> show cpu)) $ do
           let mode = digits !! (2 + n)
           case mode of
             0 -> Ptr <$> m M.!? addr
             1 -> Val <$> m M.!? addr
             2 -> Ptr . (+) (rb cpu) <$> m M.!? addr
             x -> error $ "undefined parameter mode: " <> show x

toDigits :: Int -> [Int]
toDigits = map (read . (:[])) . show

run :: CPU -> [Int]
run =  outputs . snd . run'  
  where
    run' :: CPU -> ((), CPU)
    run' = runState step
    step = do
      cpu <- get
      case next cpu of
        HALT -> eval HALT
        instruction -> eval instruction >> step

mkCPU :: [Int] -> [Int] -> CPU
mkCPU input mem = CPU (M.fromList (zip [0..] mem)) 0 input [] 0

mkCPUinit :: Int -> Int -> [Int] -> CPU
mkCPUinit n v mem = CPU (M.union (M.fromList (zip [0..] mem))  (M.fromList [(1, n), (2, v)]) ) 0 [] [] 0

day06a :: String -> String
day06a = show . run . (mkCPU [1]) . fmap read . splitOn ","

day06b :: String -> String
day06b = show . run . (mkCPU [2]) . fmap read . splitOn ","

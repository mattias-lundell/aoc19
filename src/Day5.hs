module Day5 (day05a, day05b, toDigits) where

import qualified Data.List as L
import           Data.List.Split                ( splitOn )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import Debug.Trace (trace)
import           Control.Monad.State.Lazy
import           Data.Maybe                     ( fromMaybe )


type CPUState = State CPU

data CPU = CPU { mem :: Vector Int
               , ip :: Int
               , inputs :: [Int]
               , outputs :: [Int]
               , relativeBase :: Int
               } deriving (Eq, Show)

newtype Pointer = Pointer { getPtr :: Int } deriving (Eq, Show)

data Param = Ptr Int | Val Int deriving (Eq, Show)

data Op = ADD Param Param Param
        | MUL Param Param Param
        | INPUT Param
        | OUTPUT Param
        | JZ Param Param
        | JNZ Param Param
        | CMPLT Param Param Param
        | CMPEQ Param Param Param
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
eval (JZ x y) = do
  x' <- deref x
  y' <- deref y
  if x' == 0 then setIp y' else incIp 3
eval (JNZ x y) = do
  x' <- deref x
  y' <- deref y
  if x' /= 0 then setIp y' else incIp 3
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
eval HALT = pure ()

incIp :: Int -> CPUState ()
incIp i = modify $ \s -> s { ip = ip s + i }

setIp :: Int -> CPUState ()
setIp i = modify $ \s -> s { ip = i }
  
getInput :: CPUState Int
getInput = do
  s <- get 
  let input = head (inputs s)
  modify $ \s -> s {inputs = tail (inputs s)}
  return input 

output :: Int -> CPUState ()
output i = modify $ \s -> s { outputs = i : outputs s }

set :: Param -> Int -> CPUState ()
set (Ptr p) val = modify $ \s -> s { mem = mem s V.// [(p, val)] }

deref :: Param -> CPUState Int
deref (Ptr p) =
  fromMaybe (error ("out of bounds deref of pointer: " ++ show p))
  . (V.!? p)
  . mem
  <$> get
deref (Val v) = return v

next :: CPU -> Op
next cpu = let op = next' cpu in trace (show op) op
  
next' cpu = case op of
             [1, 0] -> ADD x y z
             [2, 0] -> MUL x y z
             [3, 0] -> INPUT x
             [4, 0] -> OUTPUT x
             [5, 0] -> JNZ x y
             [6, 0] -> JZ x y
             [7, 0] -> CMPLT x y z
             [8, 0] -> CMPEQ x y z
             [9, 9] -> HALT
             x -> error $ "undefined op: " <> show cpu <> " " <> show op <> " " <> show digits
  where instruction = fromMaybe (error ("out of bounds deref of pointer: " ++ show i)) (m V.!? i)
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
             0 -> Ptr <$> m V.!? addr
             1 -> Val <$> m V.!? addr
             2 -> Ptr . (+) (relativeBase cpu) <$> m V.!? addr
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
mkCPU input mem = CPU (V.fromList mem) 0 input [] 0

mkCPUinit :: Int -> Int -> [Int] -> CPU
mkCPUinit n v mem = CPU ((V.fromList mem)V.// [(1, n), (2, v)] ) 0 [] [] 0

day05a :: String -> String
day05a = show . run . (mkCPU [1]) . fmap read . splitOn ","

day05b :: String -> String
day05b = show . run . (mkCPU [5]) . fmap read . splitOn ","

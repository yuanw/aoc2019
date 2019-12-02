module Day2 where

import           Control.Monad       (guard)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as M

data Op = Add | Multiply | Halt deriving (Eq, Show)

type Position = Int

data Progress = Progress {
    value :: V.Vector Int
  , pos   :: Position
  , op    :: Op
  } deriving (Eq, Show)

getOp :: Int -> Op
getOp i = case i of
  1     -> Add
  2     -> Multiply
  99    -> Halt
  other -> error $ "unknown op " ++ show other


initProgress :: V.Vector Int -> Progress
initProgress nums  = Progress nums 0  (getOp $ V.unsafeIndex nums 0)


operation :: (Int -> Int -> Int) ->  V.Vector Int -> Int -> Int -> Int -> V.Vector Int
operation op v from1 from2 to = V.modify (\v' -> M.write v' to (op x y) ) v
           where x = (V.!) v from1
                 y = (V.!) v from2

add :: V.Vector Int -> Position -> V.Vector Int
add v p = operation (+) v x y z
   where x = (V.!) v (p + 1)
         y = (V.!) v (p + 2)
         z = (V.!) v (p + 3)

multiply :: V.Vector Int -> Position -> V.Vector Int
multiply v p = operation (*) v x y z
   where x = (V.!) v (p + 1)
         y = (V.!) v (p + 2)
         z = (V.!) v (p + 3)

halt :: V.Vector Int -> Position -> V.Vector Int
halt = const

getOperation :: Op -> V.Vector Int -> Position -> V.Vector Int
getOperation o = case o of
  Add      -> add
  Multiply -> multiply
  Halt     -> halt

run :: Progress -> Progress
run p = if (op p) == Halt then p else  Progress v' pos' op'
  where  v = value p
         pos' = pos p + 4
         v' = (getOperation . op $ p) (value p) (pos p)
         op' = getOp (V.unsafeIndex v' pos')

fix :: Progress -> Progress
fix p = if p == p' then p' else fix p'
  where p' = run p

--TODO parse input
runResult :: Int -> Int -> Progress
runResult noun verb = fix (initProgress init'')
  where init = V.fromList [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,6,19,23,1,13,23,27,1,6,27,31,1,31,10,35,1,35,6,39,1,39,13,43,2,10,43,47,1,47,6,51,2,6,51,55,1,5,55,59,2,13,59,63,2,63,9,67,1,5,67,71,2,13,71,75,1,75,5,79,1,10,79,83,2,6,83,87,2,13,87,91,1,9,91,95,1,9,95,99,2,99,9,103,1,5,103,107,2,9,107,111,1,5,111,115,1,115,2,119,1,9,119,0,99,2,0,14,0] :: V.Vector Int
        init' = V.modify (\v' -> M.write v' 1 noun) init
        init'' = V.modify (\v' -> M.write v' 2 verb) init'


getOutout :: Progress -> Int
getOutout p = V.unsafeIndex(value p) 0

show' :: Int -> String
show' num = if num > 9 then show num else "0" ++ show num

findAnswer :: String
findAnswer = do
  x <- [0..99]
  y <- [0..99]
  guard (getOutout (runResult x y) == 19690720)
  show' x ++ show' y



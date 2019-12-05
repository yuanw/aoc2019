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


operation :: (Int -> Int -> Int) ->  V.Vector Int -> Position -> V.Vector Int
operation op v p = V.modify (\v' -> M.write v' to (op x y) ) v
           where from1 = (V.!) v p + 1
                 from2 = (V.!) v p + 2
                 to = (V.!) v p + 3
                 x = (V.!) v from1
                 y = (V.!) v from2

add :: V.Vector Int -> Position -> V.Vector Int
add = operation (+)

multiply :: V.Vector Int -> Position -> V.Vector Int
multiply = operation (*)

getOperation :: Op -> V.Vector Int -> Position -> V.Vector Int
getOperation o = case o of
  Add      -> add
  Multiply -> multiply
  Halt     -> const

run :: Progress -> Progress
run p = if op p == Halt then p else  Progress v' pos' op'
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
  where init = V.fromList [1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,13,19,1,10,19,23,2,9,23,27,1,6,27,31,1,10,31,35,1,35,10,39,1,9,39,43,1,6,43,47,1,10,47,51,1,6,51,55,2,13,55,59,1,6,59,63,1,10,63,67,2,67,9,71,1,71,5,75,1,13,75,79,2,79,13,83,1,83,9,87,2,10,87,91,2,91,6,95,2,13,95,99,1,10,99,103,2,9,103,107,1,107,5,111,2,9,111,115,1,5,115,119,1,9,119,123,2,123,6,127,1,5,127,131,1,10,131,135,1,135,6,139,1,139,5,143,1,143,9,147,1,5,147,151,1,151,13,155,1,5,155,159,1,2,159,163,1,163,6,0,99,2,0,14,0] :: V.Vector Int
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


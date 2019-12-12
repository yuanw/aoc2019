module Day5 where

import           Control.Monad       (guard)
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as M

data Op = Add | Multiply | Input | Output | Halt deriving (Eq, Show)

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
  3     -> Input
  4     -> Output
  99    -> Halt
  other -> error $ "unknown op " ++ show other

getOpLen :: Op -> Int
getOpLen Halt   = 1
getOpLen Input  = 2
getOpLen Output = 2
getOpLen _      = 4

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



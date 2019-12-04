module Day3 where

data Direction  =  North | East | South | West
  deriving (Read, Show, Eq, Ord, Enum, Bounded)


data Turn = Up | Right | Down | Left deriving (Read, Show, Eq, Ord, Enum, Bounded)

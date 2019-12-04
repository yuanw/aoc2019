module Day3 where

import qualified Data.Set as Set

data Direction = North | East | South | West
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

data Turn = U | R | D | L
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

type Point = (Int, Int)

data Wire = Wire {
    current :: Point
  , path    :: Set.Set Point} deriving Show

initWire :: Wire
initWire = Wire (0,0) $ Set.singleton (0,0)

travel :: Point -> Turn -> Int -> (Point,Set.Set Point)
travel (x,y) dir d = case dir of
   U -> ((x+d, y), Set.fromList [(x+i, y) | i <- [1..d]])
   D -> ((x-d, y), Set.fromList [(x-i, y) | i <- [1..d]])
   L ->  ((x, y-d), Set.fromList [(x, y-i) | i <- [1..d]])
   R ->  ((x, y+d), Set.fromList [(x, y+i) | i <- [1..d]])

shift :: Wire -> Turn -> Int -> Wire
shift before t distance = Wire point' (Set.union path' (path before))
    where (point', path') = travel (current before) t distance

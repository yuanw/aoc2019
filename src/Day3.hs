module Day3 where

import qualified Data.Set as Set
import           Data.List.Split (splitOn)

data Turn = U | R | D | L
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

type Point = (Int, Int)

data Move = Move {
     turn :: Turn
   , dest :: Int
   } deriving (Show)

data Wire = Wire {
    current :: Point
  , path    :: Set.Set Point
  , step :: Int } deriving Show

initWire :: Wire
initWire = Wire (0,0) (Set.singleton (0,0)) 0

travel :: Point -> Turn -> Int -> (Point,Set.Set Point)
travel (x,y) dir d = case dir of
   U -> ((x+d, y), Set.fromList [(x+i, y) | i <- [1..d]])
   D -> ((x-d, y), Set.fromList [(x-i, y) | i <- [1..d]])
   L ->  ((x, y-d), Set.fromList [(x, y-i) | i <- [1..d]])
   R ->  ((x, y+d), Set.fromList [(x, y+i) | i <- [1..d]])

shift :: Wire -> Move -> Wire
shift before m = Wire point' (Set.union path' (path before)) (step before + dest m)
    where (point', path') = travel (current before) (turn m) (dest m)

--TODO figure foldr vs foldl
crossPath :: [Move] -> [Move] -> Set.Set Point
crossPath m1 m2 = Set.intersection (path p1) (path p2)
  where p1 = foldl shift initWire m1
        p2 = foldl shift initWire m2

closestDistance :: [Move] -> [Move] -> Maybe Int
closestDistance m1 m2 = Set.lookupGT 0 . Set.fromList . map (\p -> (abs $ fst p) + (abs $ snd p)) . Set.toList  . Set.delete (0,0)  $ crossPath m1 m2

readMoves :: String -> [Move]
readMoves = map readMove . splitOn ","
  where
    readMove :: String -> Move
    readMove i = Move (read [head i]) (read $ tail i)


getMoves :: IO ([Move], [Move])
getMoves = do
    content <- readFile "./input/day3.txt"
    let moves = map readMoves $ lines content
    return (head moves, (head . tail) moves)

partI :: IO ()
partI = do
  (m1, m2) <- getMoves
  print $ closestDistance m1 m2

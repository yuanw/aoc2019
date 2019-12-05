module Day3 where

import           Data.Maybe (fromJust)
import qualified Data.Map as Map
import           Data.List.Split (splitOn)

data Turn = U | R | D | L
  deriving (Read, Show, Eq, Ord, Enum, Bounded)

type Point = (Int, Int)

data Move = Move {
     turn :: !Turn
   , dest :: !Int
   } deriving (Show)

data Wire = Wire {
    current :: Point
  , path    :: Map.Map Point Int
  , step :: !Int } deriving Show

initWire :: Wire
initWire = Wire (0,0) (Map.singleton (0,0) 0) 0

travel :: Point -> Turn -> Int -> Int -> (Point, Map.Map Point Int)
travel (x,y) dir d s = case dir of
   U -> ((x+d, y), Map.fromList [((x+i, y), s +i ) | i <- [1..d]])
   D -> ((x-d, y), Map.fromList [((x-i, y), s + i) | i <- [1..d]])
   L ->  ((x, y-d), Map.fromList [((x, y-i), s +i) | i <- [1..d]])
   R ->  ((x, y+d), Map.fromList [((x, y+i), s+i) | i <- [1..d]])

shift :: Wire -> Move -> Wire
shift before m = Wire point' (Map.union (path before)  path') (step before + dest m)
    where (point', path') = travel (current before) (turn m) (dest m) (step before)


type Find = Wire -> Wire -> [Point] -> Int

closest :: Find
closest w1 w2 crossed = minimum $ map (\ p -> (abs . fst) p + (abs . snd) p) crossed

fewest :: Find
fewest w1 w2 crossed = minimum $ map (\ p -> fromJust (Map.lookup p m1) + fromJust (Map.lookup p m2)) crossed
    where m1 = path w1
          m2 = path w2 

--TODO figure foldr vs foldl
closestDistance :: [Move] -> [Move] -> Find -> Int
closestDistance m1 m2 find = find wire1 wire2 crossed
  where wire1 = foldl shift initWire m1
        wire2 = foldl shift initWire m2
        crossed = (Map.keys . Map.filter (/= 0)) $ Map.intersection (path wire1) (path wire2)

        
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
  print $ closestDistance m1 m2 closest

partII :: IO ()
partII = do
  (m1, m2) <- getMoves
  print $ closestDistance m1 m2 fewest

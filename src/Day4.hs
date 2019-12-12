module Day4 where

import           Data.List (sort)
import qualified Data.Map  as Map

neverDecrease :: Int -> Bool
neverDecrease a = a == (read .sort. show) a

sameAdjacentDigits :: Int -> Bool
sameAdjacentDigits = same .show
  where same :: String -> Bool
        same []       = False
        same [x]      = False
        same (x:y:xs) = (x == y) || same (y:xs)

--
partI :: IO ()
partI = (print  . length)$  filter (\ a -> neverDecrease a && sameAdjacentDigits a) [134564..585159]

reduce :: Int -> Map.Map Int Int
reduce a = foldr (\c m -> Map.insertWith (+) (read [c]) 1 m) Map.empty s
  where s = show a

notPartLarge :: Int -> Bool
notPartLarge = (/= 0) . Map.size  .   Map.filter (== 2)  . reduce


partII :: IO ()
partII = (print  . length)$  filter (\ a -> neverDecrease a && sameAdjacentDigits a && notPartLarge a) [134564..585159]

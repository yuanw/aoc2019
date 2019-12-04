module Day1 (result) where

fuel :: Int -> Int
fuel x = (x `div` 3) - 2

readInput :: IO [Int]
readInput = do
  content <- readFile "./input/day1.txt"
  return $ map read $ lines content

result :: IO Int
result = fmap (sum . map (fixFuel 0)) readInput

--TODO using fix point
-- https://medium.com/@cdsmithus/fixpoints-in-haskell-294096a9fc10
fixFuel :: Int -> Int -> Int
fixFuel x y = if z > 6 then fixFuel (x + z) z  else x + z
  where z = fuel y

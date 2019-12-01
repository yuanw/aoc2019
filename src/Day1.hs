module Day1 where

fuel :: Int -> Int
fuel x = (x `div` 3) - 2

readInput :: IO [Int]
readInput = do
  content <- readFile "./input/day1.txt"
  return $ map read $ lines content

result :: IO [Int] -> IO Int
result = fmap (sum . map fixFuel)

--TODO using fix point, at least write as tail recursion
-- https://medium.com/@cdsmithus/fixpoints-in-haskell-294096a9fc10
fixFuel :: Int -> Int
fixFuel x = if y > 6 then y + fixFuel y  else y
  where y = fuel x

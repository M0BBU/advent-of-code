{-# LANGUAGE OverloadedStrings #-}

-- OverloadedStrings lets you treat Strings as a better abstraction rather
-- than [Char]?

import Data.List
import Data.Char
import Data.Text (pack, unpack, replace)

main = do
  contents <- readFile "2.txt"
  -- print contents
  -- print $ contents
  print $ part1 contents
  -- print $ part2 contents

part1 :: String -> Int
part1 s = foldl checkSafe' 0 $ readInput s
  where checkSafe' :: Int -> [Int] -> Int
        checkSafe' acc xs = acc + x
          where x =
                  case checkMonotonic xs of
                    r | r == Increasing || r == Decreasing -> 1
                    _ -> 0

-- part2 :: String -> Int
-- part2 s = getSim . sortPairs . recombine . map words $ lines s

readInput :: String -> [[Int]]
readInput s =  map (map read . words) $ lines s

data Monotonicity = Increasing | Decreasing | None
  deriving (Eq, Show)

checkMonotonic :: [Int] -> Monotonicity
-- checkMonotonic _ = None
checkMonotonic [a,b]
  | x > 0 && x <= 3 = Decreasing
  | x < 0 && x >= -3 = Increasing
  | otherwise = None
  where x = a - b
checkMonotonic (x:xs) = case checkMonotonic [x, head xs] of
  r | r == checkMonotonic xs -> r
  _ -> None

-- recombine :: [[String]] -> ([Int],[Int])
-- recombine [] = ([],[])
-- recombine ([a,b]:ss) = ([read a] ++ (fst $ recombine ss), [read b] ++ (snd $ recombine ss))

-- sortPairs :: ([Int],[Int]) -> ([Int],[Int])
-- sortPairs (a,b) = (sort a, sort b)

-- getSum :: ([Int],[Int]) -> Int
-- getSum ([],[]) = 0
-- getSum (x:xs, y:ys) = abs(x - y) + getSum (xs,ys)

-- getSim :: ([Int],[Int]) -> Int
-- getSim ([],_) = 0
-- getSim (x:xs, ys) = x * countOccur x ys + getSim (xs, ys)

-- countOccur :: Int -> [Int] -> Int
-- countOccur a = length . filter (==a)

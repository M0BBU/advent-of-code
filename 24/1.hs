{-# LANGUAGE OverloadedStrings #-}

-- OverloadedStrings lets you treat Strings as a better abstraction rather
-- than [Char]?

import Data.List
import Data.Char
import Data.Text (pack, unpack, replace)

main = do
  contents <- readFile "1.txt"
  -- print $ part1 contents
  print $ part2 contents

part1 :: String -> Int
part1 s = getSum . sortPairs . recombine . map words $ lines s

part2 :: String -> Int
part2 s = getSim . sortPairs . recombine . map words $ lines s

recombine :: [[String]] -> ([Int],[Int])
recombine [] = ([],[])
recombine ([a,b]:ss) = ([read a] ++ (fst $ recombine ss), [read b] ++ (snd $ recombine ss))

sortPairs :: ([Int],[Int]) -> ([Int],[Int])
sortPairs (a,b) = (sort a, sort b)

getSum :: ([Int],[Int]) -> Int
getSum ([],[]) = 0
getSum (x:xs, y:ys) = abs(x - y) + getSum (xs,ys)

getSim :: ([Int],[Int]) -> Int
getSim ([],_) = 0
getSim (x:xs, ys) = x * countOccur x ys + getSim (xs, ys)

countOccur :: Int -> [Int] -> Int
countOccur a = length . filter (==a)

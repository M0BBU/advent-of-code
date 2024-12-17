{-# LANGUAGE OverloadedStrings #-}

-- OverloadedStrings lets you treat Strings as a better abstraction rather
-- than [Char]?

import Data.List
import Data.Char
import Data.Ix
import Data.Text (pack, unpack, replace)

main = do
  contents <- readFile "2.txt"
  print $ part1 contents
  print $ part2 contents

part1 :: String -> Int
part1 s = length . filter (checkReport . pure . report) $ readInput s

part2 :: String -> Int
part2 s = length . filter checkReport2 $ readInput s

readInput :: String -> [[Int]]
readInput s =  map (map read . words) $ lines s

report :: [Int] -> [Int]
report xs = zipWith (-) xs (tail xs)

checkReport :: [[Int]] -> Bool
checkReport xs = or $ all . inRange <$> [(-3, -1), (1, 3)] <*> xs

checkReport2 :: [Int] -> Bool
checkReport2 xs = checkReport xs'
  where
    xs' = report <$> xs : zipWith (<>) (inits xs) (tail $ tails xs)

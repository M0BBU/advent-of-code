{-# LANGUAGE OverloadedStrings #-}

-- OverloadedStrings lets you treat Strings as a better abstraction rather
-- than [Char]?

import Data.List
import Data.Char
import Data.Text (pack, unpack, replace)

main = do
  contents <- readFile "1.txt"
  print $ part1 contents
  print $ part2 contents

part1 :: String -> Int
part1 s = foldl1 (+) $ map getNumber $ lines s

part2 :: String -> Int
part2 s = foldl1 (+) $ map getNumber $ lines (replaceWords s)

replaceWords :: String -> String
replaceWords = unpack
               . replace "nine" "nine9nine"
               . replace "eight" "eight8eight"
               . replace "seven" "seven7seven"
               . replace "six" "six6six"
               . replace "five" "five5five"
               . replace "four" "four4four"
               . replace "three" "three3three"
               . replace "two" "two2two"
               . replace "one" "one1one"
               . pack

getNumber :: String -> Int
getNumber cs = (getDigit cs) * 10 + (getDigit $ reverse cs)

getDigit :: String -> Int
getDigit (c:cs) = case isDigit c of
                     True -> digitToInt c
                     False -> getDigit cs

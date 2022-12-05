import Data.List
import Data.Char

main = do
  contents <- readFile "3.txt"
  print $ solveHard contents

-- Solve Hard --

solveHard :: String -> Int
solveHard contents =
  sum . map (getPriority . head) . map findBadge . groupThree $ lines contents

groupThree :: [String] -> [[String]]
groupThree [] = []
groupThree xs = [fst $ splitAt 3 xs] ++ groupThree(snd $ splitAt 3 xs)

findBadge :: [String] -> String
findBadge xs = foldl1 intersect xs
  



-- Solve Easy -- 
solveEasy :: String -> Int
solveEasy contents =
  sum . map (getPriority . head) . map (findCommon . halve) $ lines contents
  
halve :: String -> [String]
halve x = [take (length x `div` 2) x] ++ [take (length x `div` 2) $ reverse x]

findCommon :: [String] -> String
findCommon xs = head xs `intersect` last xs

getPriority :: Char -> Int
getPriority c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

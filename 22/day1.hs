import Data.List


main = do
  contents <- readFile "1.txt"
  print $ solveEasy contents
  print $ solveHard contents

-- Easy --
solveEasy :: String -> Int
solveEasy c = maximum . map sum $ parseInput c
----------

-- Hard --
solveHard :: String -> Int
solveHard c = sum . take 3 . reverse . sort . map sum $ parseInput c
----------

-- Helpers --
readIntList :: [String] -> [Int]
readIntList = map (read :: String -> Int)

parseInput :: String -> [[Int]]
parseInput c = map (readIntList . lines) $ parseStr c

-- Parses blocks of strings separated by two new liens
parseStr :: String -> [String]
parseStr "" = []
parseStr "\n" = []
parseStr ('\n':'\n':cs) = "\n":parseStr cs
parseStr (c:cs) = case parseStr cs of
  []   -> [[c, '\n']]
  p:ps -> (c:p):ps





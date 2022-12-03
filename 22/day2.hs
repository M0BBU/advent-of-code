
main = do
  contents <- readFile "2.txt"
  print . solveEasy $ lines contents
  print . solveHard $ lines contents


-- Solve Hard --
solveHard :: [String] -> Int
solveHard xs = sumScore2 xs

sumScore2 :: [String] -> Int
sumScore2 [""] = 0
sumScore2 (x:xs) = addScore2 x + sumScore2(xs)

addScore2 :: String -> Int
addScore2 "" = 0
addScore2 s
  | last s == 'X' = 0 + (convertVal2 . makeLose $ head s)
  | last s == 'Y' = 3 + (convertVal2 $ head s)
  | last s == 'Z' = 6 + (convertVal2 . makeWin $ head s)
  
makeLose :: Char -> Char
makeLose c
  | c == 'A' = 'C'
  | c == 'B' = 'A'
  | c == 'C' = 'B'

makeWin :: Char -> Char
makeWin c
  | c == 'A' = 'B'
  | c == 'B' = 'C'
  | c == 'C' = 'A'
  
convertVal2 :: Char -> Int
convertVal2 c
  | c == 'A' = 1
  | c == 'B' = 2
  | c == 'C' = 3
------------------------
            
-- Solve Easy --
solveEasy :: [String] -> Int
solveEasy xs = sumScore xs

sumScore :: [String] -> Int
sumScore [""] = 0
sumScore (x:xs) = addScore x + sumScore(xs)

addScore :: String -> Int
addScore "" = 0
addScore s
  | didWin s         = 6 + convertVal (last s)
  | didTie s         = 3 + convertVal (last s)
  | otherwise        = convertVal(last s)
  
didWin :: String -> Bool
didWin s
  | head s == 'A' && last s == 'Y' = True
  | head s == 'B' && last s == 'Z' = True
  | head s == 'C' && last s == 'X' = True
  | otherwise = False

didTie :: String -> Bool
didTie s
  | head s == 'A' && last s == 'X' = True
  | head s == 'B' && last s == 'Y' = True
  | head s == 'C' && last s == 'Z' = True
  | otherwise = False

  
convertVal :: Char -> Int
convertVal c
  | c == 'X' = 1
  | c == 'Y' = 2
  | c == 'Z' = 3
------------------------


main = do
  contents <- readFile "4.txt"
  print $ solveEasy contents
  print $ solveHard contents

-- Solve Hard --
solveHard :: String -> Int
solveHard contents =
  length . filter checkContains2 . map getIntervals . lines $ contents

checkContains2 :: [(Int, Int)] -> Bool
checkContains2 inter = do
  let (l0, r0) = head inter
  let (l1, r1) = last inter
  if ((l1 <= r0 && r0 <= r1) || (l0 <= r1 && r1 <= r0)) then True
  else False
-----------------

-- Solve Easy -- 
solveEasy :: String -> Int
solveEasy contents =
  length . filter checkContains . map getIntervals . lines $ contents

getIntervals :: String -> [(Int, Int)]
getIntervals contents = [readInterval $ takeWhile (/= ',') contents,
                         readInterval . drop 1 $ dropWhile (/= ',') contents]

readInterval :: String -> (Int, Int)
readInterval inter = (read $ takeWhile (/= '-') inter :: Int,
                      read . drop 1 $ dropWhile (/= '-') inter :: Int)

checkContains :: [(Int, Int)] -> Bool
checkContains inter = do
  let (l0, r0) = head inter
  let (l1, r1) = last inter
  if (l0 <= l1 && r1 <= r0) || (l1 <= l0 && r0 <= r1) then True
  else False
------------------------------

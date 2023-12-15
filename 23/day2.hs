{-# LANGUAGE OverloadedStrings #-}

-- OverloadedStrings lets you treat Strings as a better abstraction rather
-- than [Char]?

import Data.Either as E
import Data.List
import Data.Char
import Data.Text (pack, unpack, replace)
import Text.Parsec as P

data Game = Game { gid :: Int, color :: Color } deriving Show
data Color = Color { red :: Int, green :: Int, blue :: Int} deriving Show

instance Monoid Color where
  mempty = Color 0 0 0

instance Semigroup Color where
  (Color a b c) <> (Color x y z) = Color (max a x) (max b y) (max c z)

main = do
  contents <- readFile "2.txt"
  print $ part1 contents
  print $ part2 contents

part1 :: String -> Int
part1 s = sum . map gid $ filter possibleGames (parseGames s)

part2 :: String -> Int
part2 s = sum . map power $ parseGames s

power :: Game -> Int
power = valid . color
  where valid (Color r g b) = r * g * b

possibleGames  :: Game -> Bool
possibleGames = valid . color
  where valid (Color r g b) = r <= 12 && g <= 13 && b <= 14

parseGames :: String -> [Game]
parseGames g = map parseGame $ lines g

parseGame :: String -> Game
parseGame g =
  case P.parse getGame "" g of
    Left err -> error (show err)
    Right g -> g
  where
    getGame = Game <$> (P.string "Game " *> (getInt) <* P.char ':') <*> colors
    getInt = read <$> (P.many1 P.digit)
    colors = chainl color (P.char ';' >> pure(<>)) mempty
    color = chainl count (P.char ',' >> pure(<>)) mempty
    count = P.between P.space P.space getInt >>= \i ->
      Color i 0 0 <$ P.string "red" <|>
      Color 0 i 0 <$ P.string "green" <|>
      Color 0 0 i <$ P.string "blue"

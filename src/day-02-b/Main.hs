module Main where

import Data.Functor
import System.Environment
import Text.Parsec
import Text.Parsec.String (Parser)

data Color = Red | Green | Blue deriving (Show)

colorParser :: String -> Color -> Parser Color
colorParser str col = string str >> return col

colorExpr :: Parser Color
colorExpr = do
  choice
    [ colorParser "red" Red,
      colorParser "green" Green,
      colorParser "blue" Blue
    ]

data Count = Count {red :: Int, green :: Int, blue :: Int} deriving (Show)

zero :: Count
zero = Count {red = 0, green = 0, blue = 0}

applyCountPair :: Count -> (Int, Color) -> Count
applyCountPair acc (x, Red) = acc {red = red acc + x}
applyCountPair acc (x, Green) = acc {green = green acc + x}
applyCountPair acc (x, Blue) = acc {blue = blue acc + x}

numberColorExpr :: Parser (Int, Color)
numberColorExpr = do
  digits <- many1 digit
  _ <- char ' '
  color <- colorExpr

  return (read digits, color)

numberColorListExpr :: Parser [(Int, Color)]
numberColorListExpr = numberColorExpr `sepBy` string ", "

countExpr :: Parser Count
countExpr = numberColorListExpr <&> foldl applyCountPair zero

countListExpr :: Parser [Count]
countListExpr = countExpr `sepBy` string "; "

data Game = Game
  { num :: Int,
    counts :: [Count]
  }
  deriving (Show)

gameExpr :: Parser Game
gameExpr = do
  _ <- string "Game "
  digits <- many1 digit
  _ <- string ": "
  countList <- countListExpr

  return Game {num = read digits :: Int, counts = countList}

gamesExpr :: Parser [Game]
gamesExpr = gameExpr `endBy` newline

minRequiredCount :: Count -> Count -> Count
minRequiredCount acc curr =
  Count
    { red = max (red acc) (red curr),
      green = max (green acc) (green curr),
      blue = max (blue acc) (blue curr)
    }

minRequiredCountForGame :: Game -> Count
minRequiredCountForGame game = foldl minRequiredCount zero (counts game)

power :: Count -> Int
power c = red c * green c * blue c

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args

  case parse gamesExpr "" input of
    Left err -> print err
    Right games -> print $ sum $ map (power . minRequiredCountForGame) games

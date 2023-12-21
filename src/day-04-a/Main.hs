module Main where

import Data.Functor
import Data.List (intersect)
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.String (Parser)

data Card = Card
  { idx :: Int,
    winning :: [Int],
    numbers :: [Int]
  }
  deriving (Show)

intExpr :: Parser Int
intExpr = many1 digit <&> read

spaceExpr :: Parser [String]
spaceExpr = many1 (string " ")

cardExpr :: Parser Card
cardExpr = do
  _ <- string "Card"
  _ <- spaceExpr
  i <- intExpr

  _ <- string ":"
  _ <- spaceExpr
  w <- intExpr `endBy` spaceExpr

  _ <- char '|'
  _ <- spaceExpr
  n <- intExpr `sepBy` spaceExpr

  return
    Card
      { idx = i,
        winning = w,
        numbers = n
      }

cardsExpr :: Parser [Card]
cardsExpr = cardExpr `endBy` char '\n'

findWinningNumbers :: Card -> [Int]
findWinningNumbers card = winning card `intersect` numbers card

score :: Int -> Int
score 0 = 0
score c = 2 ^ (c - 1)

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args

  case parse cardsExpr "" input of
    Left err -> print err
    Right cards ->
      print $
        sum $
          map (score . length) $
            filter (not . null) $
              map findWinningNumbers cards

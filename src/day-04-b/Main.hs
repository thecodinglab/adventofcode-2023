module Main where

import Data.Functor
import Data.List (intersect)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
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

findWinningNumbers :: Card -> (Int, [Int])
findWinningNumbers card =
  (idx card, map (+ idx card) [1 .. length i])
  where
    i = winning card `intersect` numbers card

multiplier :: Map.Map Int Int -> Int -> Int
multiplier items num = fromMaybe 1 $ Map.lookup num items

incrementMultiplierBy :: Map.Map Int Int -> Int -> Int -> Map.Map Int Int
incrementMultiplierBy items num by = Map.insertWith (+) num by items

incrementMultiplier :: Map.Map Int Int -> Int -> Map.Map Int Int
incrementMultiplier items num = incrementMultiplierBy items num 1

addItemCounts :: (Int, [Int]) -> Map.Map Int Int -> Map.Map Int Int
addItemCounts (num, next) items =
  foldr
    (\x acc -> incrementMultiplierBy acc x (multiplier acc num))
    (incrementMultiplier items num)
    next

combineCards :: [(Int, [Int])] -> Map.Map Int Int
combineCards = foldr addItemCounts Map.empty

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
          combineCards $
            reverse $
              filter (not . null) $
                map findWinningNumbers cards

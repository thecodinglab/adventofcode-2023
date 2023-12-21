module Main where

import Data.Char (digitToInt, isDigit)
import Data.List (groupBy)
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import Vec

type Pos = V2 Int

-- build a map of characters and their positions based on the input grid
readInput :: Pos -> [(Pos, Char)] -> [Char] -> Map.Map Pos Char
readInput _ acc [] = Map.fromList acc
-- update y coordinate on newline
readInput (V2 _ y) acc ('\n' : xs) = readInput (V2 0 (y + 1)) acc xs
-- ignore empty spaces
readInput pos acc ('.' : xs) = readInput (pos + V2 1 0) acc xs
readInput pos acc (x : xs) = readInput (pos + V2 1 0) ((pos, x) : acc) xs

isSymbol :: Char -> Bool
isSymbol c = not $ isDigit c

isSymbolAt :: Map.Map Pos Char -> Pos -> Bool
isSymbolAt items pos = maybe False isSymbol (Map.lookup pos items)

hasSymbolNeighbor :: Map.Map Pos Char -> Pos -> Bool
hasSymbolNeighbor items pos = any (isSymbolAt items) (neighbors pos)

buildSymbolNeighborMap :: Map.Map Pos Char -> Map.Map Pos (Char, Bool)
buildSymbolNeighborMap items =
  Map.mapWithKey (\pos x -> (x, hasSymbolNeighbor items pos)) items

buildConsecutiveNumber :: [(Pos, (Int, Bool))] -> [(Pos, (Int, Bool))] -> [(Pos, (Int, Bool))]
buildConsecutiveNumber acc [] = acc
buildConsecutiveNumber [] (item : xs) = buildConsecutiveNumber [item] xs
buildConsecutiveNumber acc@((prevPos, (prevVal, prevNeigh)) : accRest) ((currPos, (currVal, currNeigh)) : xs)
  | distance == 1 =
      buildConsecutiveNumber
        ((currPos, (prevVal * 10 + currVal, prevNeigh || currNeigh)) : accRest)
        xs
  | otherwise =
      buildConsecutiveNumber
        ((currPos, (currVal, currNeigh)) : acc)
        xs
  where
    distance = manhattanDistance prevPos currPos

main :: IO ()
main = do
  args <- getArgs
  content <- readFile $ head args

  print $
    sum $
      concatMap (map fst . filter snd . map snd . buildConsecutiveNumber []) $
        groupBy (\(V2 _ y1, _) (V2 _ y2, _) -> y1 == y2) $
          map (\(pos, (val, neigh)) -> (pos, (digitToInt val, neigh))) $
            Map.toList $
              Map.filter (isDigit . fst) $
                buildSymbolNeighborMap $
                  readInput (V2 0 0) [] content

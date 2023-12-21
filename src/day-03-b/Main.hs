{-# LANGUAGE TupleSections #-}

module Main where

import Data.Char (digitToInt, isDigit)
import Data.List (groupBy, nub, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
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

findNeighboringDigits :: Map.Map Pos Char -> Pos -> [(Pos, Char)]
findNeighboringDigits items pos =
  filter (isDigit . snd) $
    mapMaybe (\p -> (p,) <$> Map.lookup p items) $
      neighbors pos

belongsToSameNumber :: Pos -> Pos -> Bool
belongsToSameNumber (V2 x1 y1) (V2 x2 y2) = y1 == y2 && (x2 - x1) == 1

countNumberNeighbors :: Map.Map Pos Char -> Pos -> Int
countNumberNeighbors items pos =
  length $
    filter (\(p, _) -> not $ any (belongsToSameNumber p) positions) digits
  where
    digits = findNeighboringDigits items pos
    positions = map fst digits

buildNeighboringNumberCountMap :: Map.Map Pos Char -> Map.Map Pos (Char, Int)
buildNeighboringNumberCountMap items =
  Map.mapWithKey (\pos x -> (x, countNumberNeighbors items pos)) items

isGear :: (Char, Int) -> Bool
isGear ('*', 2) = True
isGear _ = False

isGearAt :: Map.Map Pos (Char, Int) -> Pos -> Bool
isGearAt items pos = maybe False isGear (Map.lookup pos items)

findGearNeighbors :: Map.Map Pos (Char, Int) -> Pos -> [Pos]
findGearNeighbors items pos = filter (isGearAt items) (neighbors pos)

buildGearNeighborMap :: Map.Map Pos (Char, Int) -> Map.Map Pos (Char, [Pos])
buildGearNeighborMap items =
  Map.mapWithKey (\pos (x, _) -> (x, findGearNeighbors items pos)) items

buildConsecutiveNumber :: [(Pos, (Int, [Pos]))] -> [(Pos, (Int, [Pos]))] -> [(Pos, (Int, [Pos]))]
buildConsecutiveNumber acc [] = acc
buildConsecutiveNumber [] (item : xs) = buildConsecutiveNumber [item] xs
buildConsecutiveNumber acc@((prevPos, (prevVal, prevNeigh)) : accRest) ((currPos, (currVal, currNeigh)) : xs)
  | distance == 1 =
      buildConsecutiveNumber
        ((currPos, (prevVal * 10 + currVal, prevNeigh ++ currNeigh)) : accRest)
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
      map (product . map fst) $
        groupBy (\(_, a) (_, b) -> a == b) $
          sortBy (\(_, a) (_, b) -> compare a b) $
            concatMap (\(a, b) -> map (a,) $ nub b) $
              concatMap (filter (not . null . snd) . map snd . buildConsecutiveNumber []) $
                groupBy (\(V2 _ y1, _) (V2 _ y2, _) -> y1 == y2) $
                  map (\(pos, (val, neigh)) -> (pos, (digitToInt val, neigh))) $
                    Map.toList $
                      Map.filter (isDigit . fst) $
                        buildGearNeighborMap $
                          buildNeighboringNumberCountMap $
                            readInput (V2 0 0) [] content

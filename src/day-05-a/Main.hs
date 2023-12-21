module Main where

import Data.Functor
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.String (Parser)

data Mapping = Mapping {src :: Int, dst :: Int, len :: Int} deriving (Show)

data Environment = Environment [Int] [[Mapping]] deriving (Show)

intExpr :: Parser Int
intExpr = many1 digit <&> read

spaceExpr :: Parser String
spaceExpr = many1 (char ' ')

mappingExpr :: Parser Mapping
mappingExpr = do
  d <- intExpr
  _ <- spaceExpr
  s <- intExpr
  _ <- spaceExpr
  l <- intExpr

  return Mapping {src = s, dst = d, len = l}

mappingsExpr :: String -> Parser [Mapping]
mappingsExpr name = do
  _ <- char '\n'
  _ <- string name
  _ <- string " map:\n"
  mappingExpr `endBy` char '\n'

seedsExpr :: Parser [Int]
seedsExpr = do
  _ <- string "seeds: "
  seeds <- intExpr `sepBy` spaceExpr
  _ <- char '\n'

  return seeds

envExpr :: Parser Environment
envExpr = do
  seeds <- seedsExpr
  seedToSoil <- mappingsExpr "seed-to-soil"
  soilToFertilizer <- mappingsExpr "soil-to-fertilizer"
  fertilizerToWater <- mappingsExpr "fertilizer-to-water"
  waterToLight <- mappingsExpr "water-to-light"
  lightToTemperature <- mappingsExpr "light-to-temperature"
  temperatureToHumidity <- mappingsExpr "temperature-to-humidity"
  humidityToLocation <- mappingsExpr "humidity-to-location"

  return
    ( Environment
        seeds
        [ seedToSoil,
          soilToFertilizer,
          fertilizerToWater,
          waterToLight,
          lightToTemperature,
          temperatureToHumidity,
          humidityToLocation
        ]
    )

lookupMapping :: Mapping -> Int -> Int
lookupMapping mapping num = num - src mapping + dst mapping

lookupMappings :: [Mapping] -> Int -> Int
lookupMappings [] x = x
lookupMappings (m : ms) x
  | off >= 0 && off < len m = lookupMapping m x
  | otherwise = lookupMappings ms x
  where
    off = x - src m

applyMappings :: [[Mapping]] -> Int -> Int
applyMappings mappings x = foldr lookupMappings x (reverse mappings)

main :: IO ()
main = do
  args <- getArgs
  input <- readFile $ head args

  case parse envExpr "" input of
    Left err -> print err
    Right (Environment seeds mappings) ->
      print $
        minimum $
          map (applyMappings mappings) seeds

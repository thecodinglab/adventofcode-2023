module Main where

import Data.Char (digitToInt, isDigit)
import System.Environment (getArgs)

parseStrToInt :: String -> (Maybe Int, String)
parseStrToInt [] = (Nothing, [])
parseStrToInt ('o' : xs@('n' : 'e' : _)) = (Just 1, xs)
parseStrToInt ('t' : xs@('w' : 'o' : _)) = (Just 2, xs)
parseStrToInt ('t' : xs@('h' : 'r' : 'e' : 'e' : _)) = (Just 3, xs)
parseStrToInt ('f' : xs@('o' : 'u' : 'r' : _)) = (Just 4, xs)
parseStrToInt ('f' : xs@('i' : 'v' : 'e' : _)) = (Just 5, xs)
parseStrToInt ('s' : xs@('i' : 'x' : _)) = (Just 6, xs)
parseStrToInt ('s' : xs@('e' : 'v' : 'e' : 'n' : _)) = (Just 7, xs)
parseStrToInt ('e' : xs@('i' : 'g' : 'h' : 't' : _)) = (Just 8, xs)
parseStrToInt ('n' : xs@('i' : 'n' : 'e' : _)) = (Just 9, xs)
parseStrToInt (x : xs)
  | isDigit x = (Just (digitToInt x), xs)
  | otherwise = (Nothing, xs)

parseNumber :: String -> (Maybe Int, String)
parseNumber [] = (Nothing, [])
parseNumber str = case num of
  res@(Just _) -> (res, rest)
  _ -> parseNumber rest
  where
    (num, rest) = parseStrToInt str

parseDigits :: String -> [Int]
parseDigits [] = []
parseDigits str = case num of
  (Just x) -> x : parseDigits rest
  _ -> parseDigits rest
  where
    (num, rest) = parseNumber str

findTwoDigitNumber :: String -> Int
findTwoDigitNumber str = 10 * head nums + last nums
  where
    nums = parseDigits str

sumTwoDigitNumbers :: [String] -> Int
sumTwoDigitNumbers list = sum nums
  where
    nums = map findTwoDigitNumber list

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

main :: IO ()
main = do
  args <- getArgs
  input <- readLines $ head args
  print $ sumTwoDigitNumbers input

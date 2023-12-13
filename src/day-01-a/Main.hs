module Main where

import Data.Char (digitToInt, isDigit)
import System.Environment (getArgs)

findTwoDigitNumber :: String -> Int
findTwoDigitNumber str = 10 * head nums + last nums
  where
    nums = map digitToInt $ filter isDigit str

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

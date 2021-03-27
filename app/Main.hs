module Main where

import Day01
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args == 1
    then
      let day = read $ head args :: Int
       in case day of
            1 ->
              do
                input <- readFile "resources/day01.txt"
                print $ Day01.partOne input
                print $ Day01.partTwo input
            _ -> putStrLn "Unknown day."
    else putStrLn "Please input day number."

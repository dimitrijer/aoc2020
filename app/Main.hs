module Main where

import Day01
import Day02
import Day03
import Day04
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args == 1
    then case read $ head args :: Int of
      1 ->
        do
          input <- readFile "resources/day01.txt"
          print $ Day01.partOne input
          print $ Day01.partTwo input
      2 ->
        do
          input <- readFile "resources/day02.txt"
          print $ Day02.partOne input
          print $ Day02.partTwo input
      3 ->
        do
          input <- readFile "resources/day03.txt"
          print $ Day03.partOne input
          print $ Day03.partTwo input
      4 ->
        do
          input <- readFile "resources/day04.txt"
          print $ Day04.partOne input
          print $ Day04.partTwo input
      _ -> putStrLn "Unknown day."
    else putStrLn "Please input day number."

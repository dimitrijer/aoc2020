module Main where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import System.Environment
import Text.Printf

exec :: String -> Int -> IO ()
exec input day = do
  case day of
    1 -> do print $ Day01.partOne input; print $ Day01.partTwo input
    2 -> do print $ Day02.partOne input; print $ Day02.partTwo input
    3 -> do print $ Day03.partOne input; print $ Day03.partTwo input
    4 -> do print $ Day04.partOne input; print $ Day04.partTwo input
    5 -> do print $ Day05.partOne input; print $ Day05.partTwo input
    6 -> do print $ Day06.partOne input; print $ Day06.partTwo input
    7 -> do print $ Day07.partOne input; print $ Day07.partTwo input
    8 -> do print $ Day08.partOne input; print $ Day08.partTwo input
    _ -> do putStrLn $ "Unknown day: " ++ show day

main :: IO ()
main = do
  args <- getArgs
  if length args == 1
    then do
      let day = read $ head args
      input <- readFile $ printf "resources/day%02d.txt" day
      exec input day
    else putStrLn "Please input day number."

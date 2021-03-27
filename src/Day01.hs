module Day01 where

import Data.List (find)
import Text.Read (readMaybe)

-- Part One --------------------------------------------------------------------

type Expense = Int

type ExpenseReport = [Expense]

parseReport :: String -> Maybe ExpenseReport
parseReport s = mapM (\x -> readMaybe x :: Maybe Expense) $ lines s

combinePairs :: ExpenseReport -> [[Expense]]
combinePairs [] = []
combinePairs (x : xs) = [[x, e] | e <- xs] ++ combinePairs xs

expensesAddUp :: [Expense] -> Bool
expensesAddUp es = sum es == 2020

partOne :: String -> Maybe Int
partOne input = do
  report <- parseReport input
  (x0 : x1 : _) <- (find expensesAddUp . combinePairs) report
  return (x0 * x1)

-- Part Two --------------------------------------------------------------------

combineTriplets :: ExpenseReport -> [[Expense]]
combineTriplets [] = []
combineTriplets (x0 : xs) = [[x0, e0, e1] | (e0 : e1 : _) <- combinePairs xs] ++ combineTriplets xs

partTwo :: String -> Maybe Int
partTwo input = do
  report <- parseReport input
  (x0 : x1 : x2 : _) <- (find expensesAddUp . combineTriplets) report
  return (x0 * x1 * x2)

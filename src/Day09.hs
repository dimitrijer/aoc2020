module Day09 where

import Control.Applicative (liftA2)
import Control.Monad (msum)
import Data.List (find, sort)
import Data.Maybe (isJust)
import Day01 (combinePairs)

-- Part One --------------------------------------------------------------------

checkXMAS :: Int -> [Int] -> Maybe Int
checkXMAS p as
  | length as <= p = Nothing
  | otherwise =
    if canSum x ps
      then checkXMAS p (tail as)
      else Just x
  where
    (ps, x : _) = splitAt p as

canSum :: Int -> [Int] -> Bool
canSum x as = isJust $ find ((== x) . sum) $ combinePairs as

partOne :: String -> Maybe Int
partOne = checkXMAS 25 . map (read :: String -> Int) . lines

-- Part Two --------------------------------------------------------------------

partTwo :: String -> Maybe Int
partTwo s = do
  inv <- checkXMAS 25 as
  subl <- findSum inv as
  return $ liftA2 (+) minimum maximum subl
  where
    as = map (read :: String -> Int) . lines $ s

findSum :: Int -> [Int] -> Maybe [Int]
findSum x as = sort <$> (msum . map (findSum' []) $ it)
  where
    it = take (length as) $ iterate tail as
    findSum' :: [Int] -> [Int] -> Maybe [Int]
    findSum' toSum as'
      | x == sum toSum = Just toSum
      | x < sum toSum || null as' = Nothing
      | otherwise = findSum' (head as' : toSum) (tail as')

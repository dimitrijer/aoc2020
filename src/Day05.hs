{-# LANGUAGE LambdaCase #-}

module Day05 where

import Data.List (find, sort)
import Numeric (readInt)

-- Part One --------------------------------------------------------------------

toSeatId :: String -> Int
toSeatId s = row * 8 + col
  where
    readRow = readInt 2 (\c -> c == 'F' || c == 'B') (\c -> if c == 'B' then 1 else 0)
    readCol = readInt 2 (\c -> c == 'L' || c == 'R') (\c -> if c == 'R' then 1 else 0)
    row = (fst . head . readRow . take 7) s
    col = (fst . head . readCol . take 3 . drop 7) s

partOne :: String -> Int
partOne = foldr (max . toSeatId) 0 . lines

-- Part Two --------------------------------------------------------------------

partTwo :: String -> Maybe Int
partTwo s = do
  let ids = sort $ map toSeatId $ lines s
  (l, _) <- find (\(l, r) -> r > l + 1) $ zip ids (tail ids)
  return $ l + 1

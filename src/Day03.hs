{-# LANGUAGE LambdaCase #-}

module Day03 where

import Data.Maybe (mapMaybe)

-- PartOne ---------------------------------------------------------------------

data Tile = Empty | Tree
  deriving (Eq, Show)

type Grid = [[Tile]]

parseGrid :: String -> Grid
parseGrid = map parseRow . lines
  where
    parseRow = mapMaybe parseTile
    parseTile = \case
      '#' -> Just Tree
      '.' -> Just Empty
      _ -> Nothing

numTrees :: Grid -> Int -> Int -> Int
numTrees g r d = foldr f 0 ms
  where
    ms = moves g r d
    f = \(i, j) a -> case g !! i !! j of
      Tree -> a + 1
      Empty -> a

partOne :: String -> Int
partOne s = numTrees (parseGrid s) 3 1

-- Part Two -------------------------------------------------------------------

-- | `moves` outputs a list of moves required to traverse the grid with given slope configuration.
-- Slope configuration is in shape of right steps, down steps,
moves :: Grid -> Int -> Int -> [(Int, Int)]
moves g r d =
  [ (i, j)
    | step <- [1 .. length g - 1],
      let i = step * d,
      i < length g,
      let col = g !! i
          j = (step * r) `mod` length col
  ]

partTwo :: String -> Int
partTwo s = product $ map f [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  where
    f = uncurry $ numTrees g
    g = parseGrid s

module Day15 where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Debug.Trace

-- Part One --------------------------------------------------------------------

type Step = Int

type History = M.Map Int [Step]

speak :: Step -> History -> Step -> Int -> Int
speak end h s n
  | end == s = trace ("history size: " ++ show (length h)) n
  | otherwise =
    let s' = head $ M.findWithDefault [s - 1] n h
        n' = s - 1 - s'
     in speak end (M.insertWith (++) n [s - 1] h) (s + 1) n'

partOne :: String -> Int
partOne input = speak 2021 h s n
  where
    (h, s, n) = parse input

-- Part Two --------------------------------------------------------------------

parse :: String -> (History, Step, Int)
parse input = (h, s, n)
  where
    h = M.fromList . init $ zip ns [[i] | i <- [1 ..]]
    s = length ns + 1
    n = last ns
    ns = map read $ splitOn "," input

partTwo :: String -> Int
partTwo input = speak 30000001 h s n
  where
    (h, s, n) = parse input

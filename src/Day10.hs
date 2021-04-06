{-# LANGUAGE TupleSections #-}

module Day10 where

import Data.List (sort)
import Data.Maybe (mapMaybe)

-- Part One --------------------------------------------------------------------

parseAdapters :: String -> [Int]
parseAdapters = map (read :: String -> Int) . lines

chainAll :: [Int] -> [Int]
chainAll as = sort $ (0 : as) ++ [maximum as + 3]

partOne :: String -> Int
partOne s = length (filter (== 1) diff) * length (filter (== 3) diff)
  where
    cs = chainAll $ parseAdapters s
    diff = zipWith (-) (tail cs) cs

-- Part Two --------------------------------------------------------------------

-- | Number of combinations of `k` items from `n`-element set.
comb :: Int -> Int -> Int
comb _ 0 = 1
comb 0 _ = 0
comb n k = comb (n - 1) (k - 1) * n `div` k

-- | Chains are consecutive adapters connected together. Every chain
-- additionally stores 2 surrounding adapters, therefore chain of length k
-- stores k+1 elements.
type SingleAdapter = (Int, Int, Int)

type DoubleAdapter = (Int, Int, Int, Int)

type TripleAdapter = (Int, Int, Int, Int, Int)

makeS :: [Int] -> [SingleAdapter]
makeS xs = zip3 xs (tail xs) (tail $ tail xs)

-- | Merges consecutive single adapter chains into double adapter chains.
makeD :: [SingleAdapter] -> [DoubleAdapter]
makeD xs = mapMaybe (uncurry mergeS) $ zip xs (tail xs)
  where
    mergeS = \(x0, x1, x2) (y0, y1, y2) ->
      if x1 == y0 && x2 == y1
        then Just (x0, x1, x2, y2)
        else Nothing

-- | Merges consecutive double adapter chains into triple adapter chains.
makeT :: [DoubleAdapter] -> [TripleAdapter]
makeT xs = mapMaybe (uncurry mergeD) $ zip xs (tail xs)
  where
    mergeD = \(x0, x1, x2, x3) (y0, y1, y2, y3) ->
      if x1 == y0 && x2 == y1 && x3 == y2
        then Just (x0, x1, x2, x3, y3)
        else Nothing

canUnplugS :: SingleAdapter -> Bool
canUnplugS (x0, _, x2) = x2 - x0 <= 3

canUnplugD :: DoubleAdapter -> Bool
canUnplugD (x0, _, _, x3) = x3 - x0 <= 3

-- | Takes a number of triple adapter chains `numT`, number of single adapters
-- that can be safely plugged out `n`, and number of elements `k`, and returns
-- a number of valid combinations of `k` adapters being plugged out at the same
-- time. For more details, see test cases.
numComb :: Int -> Int -> Int -> Int
numComb numT = numComb' numT 0
  where
    numComb' nT kT n' k'
      | nT < kT || n' < 0 || k' < 0 = 0
      | otherwise = comb nT kT * comb n' k' - numComb' nT (kT + 1) (n' - 3) (k' - 3)

-- | This problem is equivalent to finding number of possible combinations of
-- adapters that can be safely plugged out at the same time. Adapters that
-- cannot possibly be plugged out on their own can safely be ignored - they
-- remain in-place.
--
-- The goal is to find sum of number of combinations of n total adapters with k
-- elements. In this context, one combination is a unique set of adapters that
-- are *unplugged* in one charging layout. However, it's a bit trickier than
-- just calling `comb n k` - even though single adapters alone are safe to plug
-- out, this may not be the case when plugging out multiple adapters at the
-- same time.
--
-- When k = 0 -> all adapters are plugged in. This is one valid charging layout
-- (C^n_0 = 1).
--
-- When k = 1 -> single adapter is unplugged. There are n such layouts (C^n_1 =
-- n), where n is the number of adapters that can be safely plugged out on
-- their own.
--
-- When k = 2 -> two adapters are unplugged. For two adapters, most
-- combinations can be unplugged *except* sequential adapters where unplugging
-- both would break <= 3 joltage requirement for surrounding adapters.
-- Sequential double chains are tracked by merging single chains. Number of
-- layouts is C^n_2 - <number of unsafe double chains>.
--
-- When k = 3 -> three adapters are unplugged. This is an interesting case
-- where *no* sequential chains of three adapters can be safely plugged out.
-- Furthermore, invalid combinations with k > 3 can be expressed in terms of
-- invalid combinations of k = 3: <num invalid triplets> * C^(n-3)_(k-3).
-- Therefore, number of valid layouts is: C^n_k - <num invalid combinations>.
partTwo :: String -> Int
partTwo s = sum [numComb numT n i | i <- [0 .. n]] - numD
  where
    ss = filter canUnplugS . makeS . chainAll . parseAdapters $ s
    n = length ss
    ds = makeD ss
    numD = length . filter (not . canUnplugD) $ ds
    -- No need to filter out invalid chains, all sequential triplets are invalid.
    numT = length $ makeT ds

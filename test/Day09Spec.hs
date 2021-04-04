module Day09Spec where

import Control.Monad
import Data.Array.IO
import Data.List (intercalate)
import Day09
import System.Random
import Test.Hspec

-- | Randomly shuffle a list (taken from https://wiki.haskell.org/Random_shuffle).
shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray' n xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray' :: Int -> [a] -> IO (IOArray Int a)
    newArray' n' xs' = newListArray (1, n') xs'

spec :: Spec
spec = do
  let ex =
        [ 35,
          20,
          15,
          25,
          47,
          40,
          62,
          55,
          65,
          95,
          102,
          117,
          150,
          182,
          127,
          219,
          299,
          277,
          309,
          576
        ]
      toS = intercalate "\n" . map show

  describe "checkXMAS" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        checkXMAS 5 ex `shouldBe` Just 127

  describe "partOne" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        x <- shuffle ([1 .. 25] :: [Int])
        let appendX = toS . (x ++) . (: [])
        partOne (appendX 49) `shouldBe` Nothing
        partOne (appendX 26) `shouldBe` Nothing
        partOne (appendX 100) `shouldBe` Just 100
        partOne (appendX 50) `shouldBe` Just 50

        y <- ([20] ++) <$> shuffle ([1 .. 19] ++ [21 .. 25] :: [Int])
        let appendY = toS . (y ++)

        partOne (appendY [45, 26]) `shouldBe` Nothing
        partOne (appendY [45, 65]) `shouldBe` Just 65
        partOne (appendY [45, 64]) `shouldBe` Nothing
        partOne (appendY [45, 63]) `shouldBe` Nothing

  describe "findSum" $ do
    context "on list that has sublist that sums to target" $ do
      it "returns that sublist (sorted)" $ do
        findSum 13 [3, 9, 1, 54, 20] `shouldBe` Just [1, 3, 9]
        findSum 13 [100, 20, 5, 9, 1, 500, 2, 7, 1, 3, 99] `shouldBe` Just [1, 2, 3, 7]

    context "on list that has no appropriate sublist" $ do
      it "returns nothing" $ do
        findSum 13 [1, 8, 94, 4, 2, 10] `shouldBe` Nothing

    context "on example input" $ do
      it "returns expected output" $ do
        findSum 127 ex `shouldBe` Just [15, 25, 40, 47]

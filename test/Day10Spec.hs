module Day10Spec where

import Data.List (delete, intercalate)
import Day10
import Test.Hspec

spec :: Spec
spec = do
  let ex = [16, 10, 15, 5, 1, 11, 7, 19, 6, 12, 4] :: [Int]
      ex' =
        [ 28,
          33,
          18,
          42,
          31,
          14,
          46,
          20,
          48,
          47,
          24,
          23,
          49,
          45,
          19,
          38,
          39,
          11,
          1,
          32,
          25,
          35,
          8,
          17,
          7,
          9,
          4,
          2,
          34,
          10,
          3
        ] ::
          [Int]

  describe "chainAll" $ do
    context "on example input" $ do
      it "returns example output" $ do
        chainAll ex `shouldBe` [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22]

  describe "partOne" $ do
    context "on example input" $ do
      it "returns example output" $ do
        partOne (intercalate "\n" (map show ex)) `shouldBe` 35
        partOne (intercalate "\n" (map show ex')) `shouldBe` 220

  describe "numComb" $ do
    context "on n = 0" $ do
      it "returns zero" $ do
        numComb 10 0 3 `shouldBe` 0
        numComb 2 0 1 `shouldBe` 0
        numComb 333 0 5 `shouldBe` 0

    context "on k = 0" $ do
      it "returns one" $ do
        numComb 10 5 0 `shouldBe` 1
        numComb 399 3 0 `shouldBe` 1
        numComb 1024 10 0 `shouldBe` 1

    context "for n adapters of 1 element" $ do
      it "returns n combinations, regardless of number of invalid triplets" $ do
        numComb 3 5 1 `shouldBe` 5
        numComb 20 43 1 `shouldBe` 43

    context "for n adapters of 2 elements" $ do
      it "returns correct number of combinations, regardless of number of invalid triplets" $ do
        numComb 9 7 2 `shouldBe` comb 7 2
        numComb 111 9 2 `shouldBe` comb 9 2

    context "for n adapters of 3 elements" $ do
      it "returns correct number of combinations, minus number of invalid triplets" $ do
        numComb 4 15 3 `shouldBe` comb 4 0 * comb 15 3 - comb 4 1 * comb 15 0

    context "for n adapters of 4 elements" $ do
      it "returns correct number of combinations, minus number of invalid triplets" $ do
        numComb 4 15 4 `shouldBe` comb 4 0 * comb 15 4 - comb 4 1 * comb 12 1

    context "for n adapters of 6 elements" $ do
      it "returns correct number of combinations, minus invalid triplets, plus unique combinations of (3) + (3) elements" $ do
        numComb 4 15 6 `shouldBe` comb 4 0 * comb 15 6 - comb 4 1 * comb 12 3 + (comb 4 2) * comb 9 0

    context "for manually generated combinations" $ do
      it "matches combinatorics result" $ do
        let as = chainAll ex'
            ss = map (\(_, x, _) -> x) . filter canUnplugS . makeS $ as
            manComb = \n -> length . filter (canUnplug as) . genComb n $ ss
        numComb 4 15 2 `shouldBe` manComb 2
        numComb 4 15 5 `shouldBe` manComb 5
        numComb 4 15 7 `shouldBe` manComb 7

  describe "partTwo" $ do
    context "on example input" $ do
      it "returns example output" $ do
        partTwo (intercalate "\n" (map show ex)) `shouldBe` 8
        partTwo (intercalate "\n" (map show ex')) `shouldBe` 19208

genComb :: Int -> [Int] -> [[Int]]
genComb 0 _ = [[]]
genComb _ [] = []
genComb n (x : xs) = map (x :) (genComb (n - 1) xs) ++ genComb n xs

canUnplug :: [Int] -> [Int] -> Bool
canUnplug as toUnplug = isSafe remaining
  where
    remaining = foldr delete as toUnplug
    isSafe = \xs -> all (\(x0, x1) -> x1 - x0 <= 3) $ zip xs (tail xs)

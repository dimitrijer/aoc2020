module Day14Spec where

import Data.Bits
import Day14
import Test.Hspec

spec :: Spec
spec = do
  let exMask = "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
      ex =
        "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n"
          ++ "mem[8] = 11\n"
          ++ "mem[7] = 101\n"
          ++ "mem[8] = 0"

  -- parseMask
  describe "parseMask" $ do
    context "on all zeros" $ do
      it "parses all-0s AND mask and all-0s OR mask" $ do
        let (mAnd, mOr) = parseMask (replicate 36 '0')
        popCount mAnd `shouldBe` 64 - 36
        countTrailingZeros mAnd `shouldBe` 36
        mOr `shouldBe` zeroBits

    context "on all ones" $ do
      it "parses all-1s AND mask and all-1s OR mask" $ do
        let (mAnd, mOr) = parseMask (replicate 36 '1')
        popCount mAnd `shouldBe` 64
        popCount mOr `shouldBe` 36
        countLeadingZeros mOr `shouldBe` 64 - 36

    context "on all Xs" $ do
      it "parses all-1s AND mask and all-0s OR mask" $ do
        let (mAnd, mOr) = parseMask (replicate 36 'X')
        mAnd `shouldBe` complement zeroBits
        mOr `shouldBe` zeroBits

    context "on example input" $ do
      it "returns expected output" $ do
        let (mAnd, mOr) = parseMask exMask
        mOr `shouldBe` 64
        testBit mAnd 1 `shouldBe` False
        popCount mAnd `shouldBe` 63

  -- partOne
  describe "partOne" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partOne ex `shouldBe` Right 165

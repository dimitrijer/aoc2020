module Day14Spec where

import qualified Control.Monad.State as S
import Data.Bits
import qualified Data.Map as M
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
      exMask' = "000000000000000000000000000000X1001X"
      exMask'' = "00000000000000000000000000000000X0XX"
      ex' =
        "mask = 000000000000000000000000000000X1001X\n"
          ++ "mem[42] = 100\n"
          ++ "mask = 00000000000000000000000000000000X0XX\n"
          ++ "mem[26] = 1"

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

  -- write'
  describe "write'" $ do
    context "on all-0s float mask" $ do
      it "executes single write" $ do
        let s0 = (M.empty, orId, orId)
        S.evalState (write' 156 133) s0 `shouldBe` M.insert 156 133 M.empty

    context "on all-0s float mask with single bit set in or mask" $ do
      it "executes single write" $ do
        let s0 = (M.empty, orId, 64)
        S.evalState (write' 156 133) s0 `shouldBe` M.insert 220 133 M.empty

    context "on float mask with one set bit" $ do
      it "executes two writes" $ do
        let s0 = (M.empty, 1, orId)
        S.evalState (write' 0 10) s0 `shouldBe` M.insert 0 10 (M.insert 1 10 M.empty)

    context "on float mask with three set bits" $ do
      it "executes eight writes" $ do
        let s0 = (M.empty, 7, orId)
        S.evalState (write' 16 234) s0
          `shouldBe` ( M.insert 16 234
                         . M.insert 17 234
                         . M.insert 18 234
                         . M.insert 19 234
                         . M.insert 20 234
                         . M.insert 21 234
                         . M.insert 22 234
                         . M.insert 23 234
                         $ M.empty
                     )
    context "on example input" $ do
      it "returns expected output" $ do
        S.evalState (setMask 33 18 >> write' 42 100) (M.empty, 0, 0)
          `shouldBe` ( M.insert 58 100
                         . M.insert 59 100
                         . M.insert 26 100
                         . M.insert 27 100
                         $ M.empty
                     )

  -- parseMask'
  describe "parseMask'" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        parseMask' exMask' `shouldBe` (33, 18)
        parseMask' exMask'' `shouldBe` (11, 0)

  -- partTwo
  describe "partTwo" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partTwo ex' `shouldBe` Right 208

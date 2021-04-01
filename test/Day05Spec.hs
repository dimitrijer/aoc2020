module Day05Spec where

import Day05
import Test.Hspec

spec :: Spec
spec = do
  -- toSeatId
  describe "toSeatId" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        toSeatId "FBFBBFFRLR" `shouldBe` 357
        toSeatId "BFFFBBFRRR" `shouldBe` 567
        toSeatId "FFFBBBFRRR" `shouldBe` 119
        toSeatId "BBFFBBFRLL" `shouldBe` 820

module Day08Spec where

import Data.List (intercalate)
import Day08
import Test.Hspec

spec :: Spec
spec = do
  let ex =
        [ "nop +0",
          "acc +1",
          "jmp +4",
          "acc +3",
          "jmp -3",
          "acc -99",
          "acc +1",
          "jmp -4",
          "acc +6"
        ]

  describe "partOne" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partOne (intercalate "\n" ex) `shouldBe` 5

  describe "partTwo" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partTwo (intercalate "\n" ex) `shouldBe` 8

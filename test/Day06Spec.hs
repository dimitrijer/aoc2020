module Day06Spec where

import Day06
import Test.Hspec

spec :: Spec
spec = do
  let concatGroups = mapGroups concat
      ex =
        "abc\n\n"
          ++ "a\n"
          ++ "b\n"
          ++ "c\n\n"
          ++ "ab\n"
          ++ "ac\n\n"
          ++ "a\n"
          ++ "a\n"
          ++ "a\n"
          ++ "a\n\n"
          ++ "b"

  describe "concatGroups" $ do
    context "on empty list" $ do
      it "returns empty list" $ do
        concatGroups [] `shouldBe` []

    context "on singleton list" $ do
      it "returns singleton list" $ do
        concatGroups ["aaa"] `shouldBe` ["aaa"]

    context "on list with no empty elements" $ do
      it "returns concatenated list" $ do
        let input = ["aaa", "zzz", "x", "cde"]
        concatGroups input `shouldBe` [concat input]

    context "on list with only empty elements" $ do
      it "returns empty list" $ do
        concatGroups [""] `shouldBe` []
        concatGroups ["", ""] `shouldBe` []

    context "on list with one empty elements" $ do
      it "returns concatenated list of two elements" $ do
        let input = ["aaa", "", "zzz", "x", "cde"]
        concatGroups input `shouldBe` ["aaa", "zzzxcde"]

  describe "partOne" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partOne ex `shouldBe` 11

  describe "partTwo" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partTwo ex `shouldBe` 6

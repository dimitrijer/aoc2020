module Day01Spec (spec) where

import Day01
import Test.Hspec

spec :: Spec
spec = do
  -- partOne tests
  describe "partOne" $ do
    context "on empty string" $ do
      it "returns nothing" $ do
        partOne "" `shouldBe` Nothing

    context "on one-element list that does not add up" $ do
      it "returns nothing" $ do
        partOne "1" `shouldBe` Nothing

    context "on one-element list that adds up" $ do
      it "returns nothing (no second element)" $ do
        partOne "2020" `shouldBe` Nothing

    context "on two-element list that adds up" $ do
      it "returns product of both elements" $ do
        partOne "2018\n2" `shouldBe` Just 4036

  -- parseReport tests
  describe "parseReport" $ do
    context "on empty string" $ do
      it "returns just an empty list" $ do
        parseReport "" `shouldBe` Just []

    context "on one number string" $ do
      it "returns single element list" $ do
        parseReport "230" `shouldBe` Just [230]

    context "on newline separated string with N numbers" $ do
      it "returns list with N ints" $ do
        parseReport "30\n-4\n111345\n98" `shouldBe` Just [30, -4, 111345, 98]

    context "on malformatted string" $ do
      it "returns nothing" $ do
        parseReport "a1aabbb3" `shouldBe` Nothing

    context "on newline separated malformatted string" $ do
      it "returns nothing" $ do
        parseReport "1\n3\n5a\n6" `shouldBe` Nothing

  -- combineTriplets test
  describe "combineTriplets" $ do
    context "on empty list" $ do
      it "returns empty list" $
        combineTriplets [] `shouldBe` []

    context "on list with single element" $ do
      it "returns empty list" $
        combineTriplets [1] `shouldBe` []

    context "on list with two elements" $ do
      it "returns empty list" $
        combineTriplets [1, 1] `shouldBe` []

    context "on list with three elements" $ do
      it "returns a single-element list" $
        combineTriplets [1, 2, 3] `shouldBe` [[1, 2, 3]]

    context "on list with four elements" $ do
      it "returns a valid combination list" $
        combineTriplets [1, 5, 8, 13] `shouldBe` [[1, 5, 8], [1, 5, 13], [1, 8, 13], [5, 8, 13]]

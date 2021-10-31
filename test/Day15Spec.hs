module Day15Spec where

import Day15
import Test.Hspec

spec :: Spec
spec = do
  -- partOne
  describe "partOne" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partOne "0,3,6" `shouldBe` 436
        partOne "1,3,2" `shouldBe` 1
        partOne "2,1,3" `shouldBe` 10
        partOne "1,2,3" `shouldBe` 27
        partOne "2,3,1" `shouldBe` 78
        partOne "3,2,1" `shouldBe` 438
        partOne "3,1,2" `shouldBe` 1836

  -- partTwo
  describe "partTwo" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partTwo "0,3,6" `shouldBe` 175594
        partTwo "1,3,2" `shouldBe` 2578
        partTwo "2,1,3" `shouldBe` 3544142
        partTwo "1,2,3" `shouldBe` 261214
        partTwo "2,3,1" `shouldBe` 6895259
        partTwo "3,2,1" `shouldBe` 18
        partTwo "3,1,2" `shouldBe` 362

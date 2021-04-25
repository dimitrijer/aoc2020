module Day13Spec where

import Day13
import Test.Hspec

spec :: Spec
spec = do
  let ex = "939\n7,13,x,x,59,x,31,19"

  -- partOne
  describe "partOne" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partOne ex `shouldBe` Right 295

  -- solve
  describe "solve" $ do
    context "on single equation" $ do
      it "returns expected output" $ do
        take 3 (solve [(13, 0)]) `shouldBe` [13, 26, 39]
        take 5 (solve [(7, 2)]) `shouldBe` [5, 12, 19, 26, 33]

    context "on two equations" $ do
      it "returns expected output" $ do
        take 3 (solve [(17, 0), (13, 2)]) `shouldBe` [102, 323, 544]

  -- partTwo
  describe "partTwo" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partTwo ex `shouldBe` Right 1068781
        partTwo "1\n17,x,13,19" `shouldBe` Right 3417
        partTwo "299\n67,7,59,61" `shouldBe` Right 754018
        partTwo "355899\n67,x,7,59,61" `shouldBe` Right 779210
        partTwo "0\n67,7,x,59,61" `shouldBe` Right 1261476
        partTwo "5\n1789,37,47,1889" `shouldBe` Right 1202161486

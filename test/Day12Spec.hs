module Day12Spec where

import qualified Control.Monad.State as S
import Day12
import Test.Hspec

spec :: Spec
spec = do
  let s0 = ((0, 0), E)
      s0' = ((0, 0), (10, 1))
      s0'' = ((0, 0), (-5, 4))
      ex = "F10\nN3\nF7\nR90\nF11"

  -- move
  describe "move" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        S.evalState (move W 10) s0 `shouldBe` (-10, 0)
        S.evalState (move S 233) s0 `shouldBe` (0, -233)
        S.evalState (move E 0) s0 `shouldBe` (0, 0)
        S.evalState (move E 30) s0 `shouldBe` (30, 0)
        S.evalState (move N 50) s0 `shouldBe` (0, 50)

  -- rotate
  describe "rotate" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        S.execState (rotate R 90) s0 `shouldBe` ((0, 0), S)
        S.execState (rotate L 90) s0 `shouldBe` ((0, 0), N)
        S.execState (rotate R 180) s0 `shouldBe` ((0, 0), W)
        S.execState (rotate L 180) ((0, 0), W) `shouldBe` ((0, 0), E)
        S.execState (rotate R 180) ((0, 0), W) `shouldBe` ((0, 0), E)
        S.execState (rotate L 90) ((0, 0), W) `shouldBe` ((0, 0), S)
        S.execState (rotate R 270) ((0, 0), W) `shouldBe` ((0, 0), S)

  -- forward
  describe "forward" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        S.execState (forward 5) s0 `shouldBe` ((5, 0), E)
        S.execState (rotate L 90 >> forward 10) s0 `shouldBe` ((0, 10), N)

    context "on sailing in circles" $ do
      it "the boat returns to starting point" $ do
        S.execState
          ( forward 10
              >> rotate L 90
              >> forward 10
              >> rotate L 90
              >> forward 10
              >> rotate L 90
              >> forward 10
              >> rotate L 90
          )
          s0
          `shouldBe` s0

  -- partOne
  describe "partOne" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partOne ex `shouldBe` Right 25

  -- rotate'
  describe "rotate'" $ do
    context "on zero rotation" $ do
      it "returns the same result" $ do
        S.execState (rotate' R 0) s0' `shouldBe` s0'
        S.execState (rotate' L 0) s0' `shouldBe` s0'
        S.execState (rotate' R 0) s0'' `shouldBe` s0''
        S.execState (rotate' L 0) s0'' `shouldBe` s0''

    context "on example input" $ do
      it "returns expected output" $ do
        S.execState (rotate' R 90) s0' `shouldBe` ((0, 0), (1, -10))
        S.execState (rotate' L 90) s0' `shouldBe` ((0, 0), (-1, 10))
        S.execState (rotate' R 180) s0' `shouldBe` ((0, 0), (-10, -1))
        S.execState (rotate' L 90) s0'' `shouldBe` ((0, 0), (-4, -5))
        S.execState (rotate' L 180) s0'' `shouldBe` ((0, 0), (5, -4))
        S.execState (rotate' R 180) s0'' `shouldBe` ((0, 0), (5, -4))
        S.execState (rotate' R 270) s0'' `shouldBe` ((0, 0), (-4, -5))

  -- partTwo
  describe "partTwo" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partTwo ex `shouldBe` Right 286

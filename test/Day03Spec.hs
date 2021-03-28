module Day03Spec (spec) where

import Day03
import Test.Hspec

spec :: Spec
spec =
  let ex =
        "..##.......\n"
          ++ "#...#...#..\n"
          ++ ".#....#..#.\n"
          ++ "..#.#...#.#\n"
          ++ ".#...##..#.\n"
          ++ "..#.##.....\n"
          ++ ".#.#.#....#\n"
          ++ ".#........#\n"
          ++ "#.##...#...\n"
          ++ "#...##....#\n"
          ++ ".#..#...#.#\n"
   in do
        -- partOne
        describe "partOne" $ do
          context "on example input" $ do
            it "returns expected output" $ do
              partOne ex `shouldBe` 7

        -- partTwo
        describe "partTwo" $ do
          context "on example input" $ do
            it "returns expected output" $ do
              partTwo ex `shouldBe` 336

module Day07Spec (spec) where

import Data.Either
import Data.List (intercalate)
import Day07
import Test.Hspec
import Text.Parsec (parse)

spec :: Spec
spec = do
  let ex =
        [ "light red bags contain 1 bright white bag, 2 muted yellow bags.",
          "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
          "bright white bags contain 1 shiny gold bag.",
          "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
          "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
          "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
          "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
          "faded blue bags contain no other bags.",
          "dotted black bags contain no other bags."
        ]
      ex2 =
        [ "shiny gold bags contain 2 dark red bags.",
          "dark red bags contain 2 dark orange bags.",
          "dark orange bags contain 2 dark yellow bags.",
          "dark yellow bags contain 2 dark green bags.",
          "dark green bags contain 2 dark blue bags.",
          "dark blue bags contain 2 dark violet bags.",
          "dark violet bags contain no other bags."
        ]
      whiteBagId = BagId "bright white"
      greenBagId = BagId "olive green"
      redBagId = BagId "maroon red"
      blueBagId = BagId "pidgeon blue"
      emptyWhiteBag =
        Bag
          { bagId = whiteBagId,
            contents = []
          }
      singletonBlueBag =
        Bag
          { bagId = blueBagId,
            contents = [(whiteBagId, 1)]
          }
      singletonRedBag =
        Bag
          { bagId = redBagId,
            contents = [(greenBagId, 3)]
          }
      greenBag =
        Bag
          { bagId = greenBagId,
            contents = [(whiteBagId, 2), (blueBagId, 5)]
          }
      allBags = [emptyWhiteBag, singletonBlueBag, singletonRedBag, greenBag]
      splode' :: [Bag] -> Bag -> Bag
      splode' bs = splode bs (\_ -> True)

  -- bagIdP
  describe "bagIdP" $ do
    context "on valid input" $ do
      it "returns expected output" $ do
        parse bagIdP "" "light red bags" `shouldBe` Right (BagId "light red")
        parse bagIdP "" "shiny gold bags" `shouldBe` Right (BagId "shiny gold")
        parse bagIdP "" "muted yellow bag" `shouldBe` Right (BagId "muted yellow")

    context "on invalid input" $ do
      it "returns parsing error" $ do
        parse bagIdP "" "faded bags" `shouldSatisfy` isLeft

  -- bagP
  describe "bagP" $ do
    context "on valid input" $ do
      it "returns expected output" $ do
        parse bagP "" (head ex)
          `shouldBe` Right
            ( Bag
                (BagId "light red")
                [(BagId "bright white", 1), (BagId "muted yellow", 2)]
            )

        parse bagP "" (last ex) `shouldBe` Right (Bag (BagId "dotted black") [])

    context "on invalid input" $ do
      it "returns parsing error" $ do
        parse bagP "" "light red bags contain." `shouldSatisfy` isLeft

  -- contains
  describe "contains" $ do
    context "on empty bag" $ do
      it "returns false for any bag ID" $ do
        contains greenBagId emptyWhiteBag `shouldBe` False
        contains redBagId emptyWhiteBag `shouldBe` False
        contains blueBagId emptyWhiteBag `shouldBe` False

      it "returns false for same bag ID" $ do
        contains whiteBagId emptyWhiteBag `shouldBe` False

    context "on singleton bag" $ do
      it "returns true only for contained bag ID" $ do
        contains greenBagId singletonRedBag `shouldBe` True
        contains blueBagId singletonRedBag `shouldBe` False
        contains whiteBagId singletonRedBag `shouldBe` False
        contains redBagId singletonRedBag `shouldBe` False

    context "on regular bag" $ do
      it "returns true for contained bag IDs" $ do
        contains greenBagId greenBag `shouldBe` False
        contains blueBagId greenBag `shouldBe` True
        contains whiteBagId greenBag `shouldBe` True
        contains redBagId greenBag `shouldBe` False

  -- findById
  describe "findById" $ do
    context "on empty list of bags" $ do
      it "returns nothing for any bag ID" $ do
        findById [] redBagId `shouldBe` Nothing

    context "on singleton list of bags" $ do
      it "returns bag for that one ID" $ do
        findById [singletonRedBag] redBagId `shouldBe` Just singletonRedBag
        findById [singletonRedBag] blueBagId `shouldBe` Nothing

    context "on regular list of bags" $ do
      it "returns bag for provided ID" $ do
        let bs = [emptyWhiteBag, greenBag]
        findById bs greenBagId `shouldBe` Just greenBag
        findById bs whiteBagId `shouldBe` Just emptyWhiteBag
        findById bs blueBagId `shouldBe` Nothing
        findById bs redBagId `shouldBe` Nothing

  -- splode . reduce (too lazy to write separate unit tests)
  describe "reduce . splode" $ do
    context "on empty bag" $ do
      it "returns the same bag" $ do
        splode' allBags emptyWhiteBag `shouldBe` emptyWhiteBag

    context "on empty list of bags" $ do
      it "returns the same bag" $ do
        splode' [] greenBag `shouldBe` greenBag

    context "on bag that only contains empty bags" $ do
      it "returns the same bag" $ do
        splode' allBags singletonBlueBag `shouldBe` singletonBlueBag

    context "on bag that contains non-empty bags" $ do
      it "returns bag with same ID but expanded contents with reduced counts" $ do
        reduce (splode' allBags greenBag)
          `shouldBe` Bag
            { bagId = greenBagId,
              contents = [(whiteBagId, 7)]
            }

        reduce (splode' allBags singletonRedBag)
          `shouldBe` Bag
            { bagId = redBagId,
              contents = [(whiteBagId, 21)]
            }

  -- partOne
  describe "partOne" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partOne (intercalate "\n" ex) `shouldBe` 4

  -- partTwo
  describe "partTwo" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partTwo (intercalate "\n" ex) `shouldBe` Just 32
        partTwo (intercalate "\n" ex2) `shouldBe` Just 126

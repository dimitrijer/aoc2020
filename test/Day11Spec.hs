module Day11Spec where

import Day11
import Test.Hspec

spec :: Spec
spec = do
  let exS =
        "L.LL.LL.LL\n"
          ++ "LLLLLLL.LL\n"
          ++ "L.L.L..L..\n"
          ++ "LLLL.LL.LL\n"
          ++ "L.LL.LL.LL\n"
          ++ "L.LLLLL.LL\n"
          ++ "..L.L.....\n"
          ++ "LLLLLLLLLL\n"
          ++ "L.LLLLLL.L\n"
          ++ "L.LLLLL.LL"
      ex = parseL exS
      stableEx =
        parseL
          ( "#.#L.L#.##\n"
              ++ "#LLL#LL.L#\n"
              ++ "L.#.L..#..\n"
              ++ "#L##.##.L#\n"
              ++ "#.#L.LL.LL\n"
              ++ "#.#L#L#.##\n"
              ++ "..L.L.....\n"
              ++ "#L#L##L#L#\n"
              ++ "#.LLLLLL.L\n"
              ++ "#.#L#L#.##"
          )

  -- neighbours
  describe "neighbours" $ do
    context "in regular case" $ do
      it "returns expected result" $ do
        neighbours ex (2, 5)
          `shouldBe` Layout
            { rows = 3,
              cols = 3,
              ps =
                [ [Just Empty, Just Empty, Just Empty],
                  [Just Empty, Just Floor, Just Floor],
                  [Just Floor, Just Empty, Just Empty]
                ]
            }

    context "on edges" $ do
      it "returns nothing outside of layout" $ do
        neighbours ex (0, 0)
          `shouldBe` Layout
            { rows = 3,
              cols = 3,
              ps =
                [ [Nothing, Nothing, Nothing],
                  [Nothing, Just Empty, Just Floor],
                  [Nothing, Just Empty, Just Empty]
                ]
            }

        neighbours ex (3, 9)
          `shouldBe` Layout
            { rows = 3,
              cols = 3,
              ps =
                [ [Just Floor, Just Floor, Nothing],
                  [Just Empty, Just Empty, Nothing],
                  [Just Empty, Just Empty, Nothing]
                ]
            }

  -- evolve
  describe "evolve" $ do
    context "on example input" $ do
      it "produces expected output" $ do
        let es = iterate (evolve evolveP) ex
            nth = \n -> head . take 1 . drop n $ es

        nth 1
          `shouldBe` parseL
            ( "#.##.##.##\n"
                ++ "#######.##\n"
                ++ "#.#.#..#..\n"
                ++ "####.##.##\n"
                ++ "#.##.##.##\n"
                ++ "#.#####.##\n"
                ++ "..#.#.....\n"
                ++ "##########\n"
                ++ "#.######.#\n"
                ++ "#.#####.##"
            )

        nth 2
          `shouldBe` parseL
            ( "#.LL.L#.##\n"
                ++ "#LLLLLL.L#\n"
                ++ "L.L.L..L..\n"
                ++ "#LLL.LL.L#\n"
                ++ "#.LL.LL.LL\n"
                ++ "#.LLLL#.##\n"
                ++ "..L.L.....\n"
                ++ "#LLLLLLLL#\n"
                ++ "#.LLLLLL.L\n"
                ++ "#.#LLLL.##"
            )

        nth 3
          `shouldBe` parseL
            ( "#.##.L#.##\n"
                ++ "#L###LL.L#\n"
                ++ "L.#.#..#..\n"
                ++ "#L##.##.L#\n"
                ++ "#.##.LL.LL\n"
                ++ "#.###L#.##\n"
                ++ "..#.#.....\n"
                ++ "#L######L#\n"
                ++ "#.LL###L.L\n"
                ++ "#.#L###.##"
            )

        nth 4
          `shouldBe` parseL
            ( "#.#L.L#.##\n"
                ++ "#LLL#LL.L#\n"
                ++ "L.L.L..#..\n"
                ++ "#LLL.##.L#\n"
                ++ "#.LL.LL.LL\n"
                ++ "#.LL#L#.##\n"
                ++ "..L.L.....\n"
                ++ "#L#LLLL#L#\n"
                ++ "#.LLLLLL.L\n"
                ++ "#.#L#L#.##"
            )

        nth 5 `shouldBe` stableEx
        nth 6 `shouldBe` stableEx
        nth 30 `shouldBe` stableEx

  -- partOne
  describe "partOne" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partOne exS `shouldBe` 37

  -- countOccupied
  describe "countOccupied" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        countOccupied
          ( parseL
              ( ".......#.\n"
                  ++ "...#.....\n"
                  ++ ".#.......\n"
                  ++ ".........\n"
                  ++ "..#L....#\n"
                  ++ "....#....\n"
                  ++ ".........\n"
                  ++ "#........\n"
                  ++ "...#....."
              )
          )
          (4, 3)
          `shouldBe` 8

        countOccupied
          ( parseL
              ( ".............\n"
                  ++ ".L.L.#.#.#.#.\n"
                  ++ "............."
              )
          )
          (1, 1)
          `shouldBe` 0

        countOccupied
          ( parseL
              ( ".##.##.\n"
                  ++ "#.#.#.#\n"
                  ++ "##...##\n"
                  ++ "...L...\n"
                  ++ "##...##\n"
                  ++ "#.#.#.#\n"
                  ++ ".##.##."
              )
          )
          (3, 3)
          `shouldBe` 0

  -- partTwo
  describe "partTwo" $ do
    context "on example input" $ do
      it "returns expected output" $ do
        partTwo exS `shouldBe` 26

module Day02Spec (spec) where

import Data.Either (isLeft)
import Day02
import Test.Hspec

spec :: Spec
spec = do
  -- countChar
  describe "countChar" $ do
    context "on empty string" $ do
      it "returns 0 for any char" $ do
        countChar 'a' "" `shouldBe` 0
        countChar 'X' "" `shouldBe` 0
        countChar '\n' "" `shouldBe` 0

    context "on a single-character string" $ do
      it "returns 1 for that character, 0 for other characters" $ do
        countChar 'A' "A" `shouldBe` 1
        countChar 'b' "A" `shouldBe` 0
        countChar 'a' "A" `shouldBe` 0

    context "on regular string" $ do
      it "returns correct value for specific char" $ do
        let s = "xor is a regular instruction, exclusive or"
        countChar 'w' s `shouldBe` 0
        countChar 'x' s `shouldBe` 2
        countChar 'o' s `shouldBe` 3

  -- parsePasswordEntry
  describe "parsePasswordEntry" $ do
    context "on regular password entry string" $ do
      it "returns a well-parsed password entry" $ do
        parsePasswordEntry "3-6 a: brtokrpdsgko"
          `shouldBe` Right
            PasswordEntry
              { policyRange = (3, 6),
                policyChar = 'a',
                password = "brtokrpdsgko"
              }

        parsePasswordEntry "1-5 P: onomathopoeia"
          `shouldBe` Right
            PasswordEntry
              { policyRange = (1, 5),
                policyChar = 'P',
                password = "onomathopoeia"
              }
        parsePasswordEntry "30-134  : ara kk k"
          `shouldBe` Right
            PasswordEntry
              { policyRange = (30, 134),
                policyChar = ' ',
                password = "ara kk k"
              }
        parsePasswordEntry "517-0 #: l33th4x"
          `shouldBe` Right
            PasswordEntry
              { policyRange = (517, 0),
                policyChar = '#',
                password = "l33th4x"
              }

    context "on invalid password entry string" $ do
      it "returns parse error" $ do
        parsePasswordEntry "" `shouldSatisfy` isLeft
        parsePasswordEntry "3a-5" `shouldSatisfy` isLeft
        parsePasswordEntry "-2 a: bbb" `shouldSatisfy` isLeft
        parsePasswordEntry "2-2 i oor" `shouldSatisfy` isLeft
        parsePasswordEntry "5- 2 P: oor" `shouldSatisfy` isLeft
        parsePasswordEntry "9*-1 P: oor" `shouldSatisfy` isLeft

    -- isValid
    describe "isValid" $ do
      context "on valid passwords" $
        do
          it "returns true when limit is (1, 3) with 3 same chars" $
            do
              isValid
                PasswordEntry
                  { policyRange = (1, 3),
                    policyChar = 'a',
                    password = "abbbaaxxx"
                  }
                `shouldBe` True

          it "returns true when limit is (1, 3) with 2 same chars" $
            do
              isValid
                PasswordEntry
                  { policyRange = (1, 3),
                    policyChar = 'a',
                    password = "bbbaxxxa"
                  }
              `shouldBe` True

          it "returns true when limit is (1, 3) with 1 char" $
            do
              isValid
                PasswordEntry
                  { policyRange = (1, 3),
                    policyChar = 'a',
                    password = "bbbxxax"
                  }
                `shouldBe` True

          it "returns true when limit is (2, 2) with 2 chars" $
            do
              isValid
                PasswordEntry
                  { policyRange = (2, 2),
                    policyChar = 'a',
                    password = "bbbaaxx"
                  }
              `shouldBe` True

      context "on passwords where character is repeated too many times" $
        do
          it "returns false" $ do
            isValid
              PasswordEntry
                { policyRange = (1, 3),
                  policyChar = 'a',
                  password = "bbbaaaxxaaa"
                }
              `shouldBe` False

      context "on passwords where character is repeated too few times" $
        do
          it "returns false" $ do
            isValid
              PasswordEntry
                { policyRange = (1, 3),
                  policyChar = 'a',
                  password = "bbbooop"
                }
              `shouldBe` False

    -- isValid'
    describe "isValid'" $ do
      context "on valid passwords" $ do
        it "return true" $ do
          isValid'
            PasswordEntry
              { policyRange = (1, 3),
                policyChar = 'a',
                password = "abcde"
              }
            `shouldBe` True

          isValid'
            PasswordEntry
              { policyRange = (0, 9),
                policyChar = 'c',
                password = "ccccccccc"
              }
            `shouldBe` True

      context "on invalid passwords" $ do
        it "returns false" $ do
          isValid'
            PasswordEntry
              { policyRange = (1, 3),
                policyChar = 'b',
                password = "cdefg"
              }
            `shouldBe` False

          isValid'
            PasswordEntry
              { policyRange = (2, 9),
                policyChar = 'c',
                password = "ccccccccc"
              }
            `shouldBe` False

          isValid'
            PasswordEntry
              { policyRange = (0, 15),
                policyChar = 'c',
                password = "ccccccccc"
              }
            `shouldBe` False

    -- partOne
    let ex = "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc"
    describe "partOne" $ do
      context "on example input" $ do
        it "returns expected output" $ do
          partOne ex `shouldBe` 2

    describe "partTwo" $ do
      context "on example input" $ do
        it "returns expected output" $ do
          partTwo ex `shouldBe` 1

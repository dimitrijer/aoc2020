{-# LANGUAGE LambdaCase #-}

module Day04Spec where

import Data.Either
import Day04
import Test.Hspec
import Text.Parsec (parse)

spec :: Spec
spec =
  let ex =
        "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n"
          ++ "byr:1937 iyr:2017 cid:147 hgt:183cm\n\n"
          ++ "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n"
          ++ "hcl:#cfa07d byr:1929\n\n"
          ++ "hcl:#ae17e1 iyr:2013\n"
          ++ "eyr:2024\n"
          ++ "ecl:brn pid:760753108 byr:1931\n"
          ++ "hgt:179cm\n\n"
          ++ "hcl:#cfa07d eyr:2025 pid:166559648\n"
          ++ "iyr:2011 ecl:brn hgt:59in"
      exInvalid =
        "eyr:1972 cid:100\n"
          ++ "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\n"
          ++ "iyr:2019\n"
          ++ "hcl:#602927 eyr:1967 hgt:170cm\n"
          ++ "ecl:grn pid:012533040 byr:1946\n\n"
          ++ "hcl:dab227 iyr:2012\n"
          ++ "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\n"
          ++ "hgt:59cm ecl:zzz\n"
          ++ "eyr:2038 hcl:74454a iyr:2023\n"
          ++ "pid:3556412378 byr:2007"
      exValid =
        "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n"
          ++ "hcl:#623a2f\n\n"
          ++ "eyr:2029 ecl:blu cid:129 byr:1989\n"
          ++ "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\n"
          ++ "hcl:#888785\n"
          ++ "hgt:164cm byr:2001 iyr:2015 cid:88\n"
          ++ "pid:545766238 ecl:hzl\n"
          ++ "eyr:2022\n\n"
          ++ "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
      parsePassports = parse passportsP ""
      parsePassports' = parse passportsP' ""
   in do
        -- passportsP
        describe "passportP" $ do
          context "on empty input" $
            it "returns a list with an empty passport" $
              parsePassports "" `shouldBe` Right [[]]

          context "on single field" $
            it "returns one passport with one field" $
              parsePassports "iyr:2020" `shouldBe` Right [[IssueYear]]

          context "on two same fields separated by space" $
            it "returns one passport with both fields" $
              parsePassports "iyr:2020 iyr:1003"
                `shouldBe` Right [[IssueYear, IssueYear]]

          context "on several fields separated by mix of space and newlines" $
            it "returns one passport with specified fields in same order" $
              parsePassports "iyr:2020 ecl:#ff3322 hcl:amb\ncid:10"
                `shouldBe` Right [[IssueYear, EyeColor, HairColor, CountryId]]

          context "on two field lists separated by newline" $
            it "returns two passports" $
              parsePassports "eyr:1930\necl:#ff3322 hgt:45in\n\ncid:10\npid:1010"
                `shouldBe` Right
                  [ [ExpirationYear, EyeColor, Height],
                    [CountryId, PassportId]
                  ]

        -- partOne
        describe "partOne" $
          context "on example passport data" $
            it "returns expected result" $ do
              partOne ex `shouldBe` 2

        -- passportsP'
        describe "passportsP'" $ do
          context "on empty input" $
            it "returns a list with an empty passport" $
              parsePassports' "" `shouldBe` Right [[]]

          context "on invalid field" $
            it "returns one passport with invalid field" $ do
              -- examples
              parsePassports' "byr:2003" `shouldBe` Right [[]]
              parsePassports' "hgt:190in" `shouldBe` Right [[]]
              parsePassports' "hgt:190" `shouldBe` Right [[]]
              parsePassports' "hcl:#123abz" `shouldBe` Right [[]]
              parsePassports' "hcl:123abc" `shouldBe` Right [[]]
              parsePassports' "ecl:wat" `shouldBe` Right [[]]
              parsePassports' "pid:0123456789" `shouldBe` Right [[]]

              -- additional tests
              parsePassports' "pid:383" `shouldBe` Right [[]]
              parsePassports' "eyr:20a byr:3032" `shouldBe` Right [[]]

          context "on valid field" $
            it "returns one passport with valid field" $ do
              parsePassports' "byr:2002" `shouldBe` Right [[BirthYear]]
              parsePassports' "hgt:60in" `shouldBe` Right [[Height]]
              parsePassports' "hgt:190cm" `shouldBe` Right [[Height]]
              parsePassports' "hcl:#123abc" `shouldBe` Right [[HairColor]]
              parsePassports' "ecl:brn" `shouldBe` Right [[EyeColor]]
              parsePassports' "pid:000000001" `shouldBe` Right [[PassportId]]

          context "on example invalid passports" $
            it "returns all invalid passports" $ do
              let f = \case
                    Left _ -> False
                    Right ps -> not (any isValid ps)
              parsePassports' exInvalid `shouldSatisfy` f

          context "on example valid passports" $
            it "returns all valid passports" $ do
              let f = \case
                    Left _ -> False
                    Right ps -> all isValid ps
              parsePassports' exValid `shouldSatisfy` f

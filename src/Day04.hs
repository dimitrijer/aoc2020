module Day04 where

import Data.List (nub)
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.String (Parser)

-- Part One --------------------------------------------------------------------

data PassportField
  = BirthYear
  | IssueYear
  | ExpirationYear
  | Height
  | HairColor
  | EyeColor
  | PassportId
  | CountryId
  deriving (Eq, Show)

type Passport = [PassportField]

keyValueP :: String -> Parser String
keyValueP s = string s *> char ':' *> many1 (alphaNum <|> char '#')

fieldP :: Parser PassportField
fieldP =
  BirthYear <$ keyValueP "byr"
    <|> IssueYear <$ keyValueP "iyr"
    <|> try (EyeColor <$ keyValueP "ecl")
    <|> ExpirationYear <$ keyValueP "eyr"
    <|> try (Height <$ keyValueP "hgt")
    <|> HairColor <$ keyValueP "hcl"
    <|> PassportId <$ keyValueP "pid"
    <|> CountryId <$ keyValueP "cid"

passportP :: Parser Passport
passportP = fieldP `sepEndBy` space

passportsP :: Parser [Passport]
passportsP = passportP `sepBy` newline <* eof

isCountryId :: PassportField -> Bool
isCountryId CountryId = True
isCountryId _ = False

isValid :: Passport -> Bool
isValid p =
  let set = nub p
   in length set == 8 || length set == 7 && not (any isCountryId p)

parseAndCountValid :: Parser [Passport] -> String -> Int
parseAndCountValid p s = case parse p "" s of
  Right ps -> length $ filter isValid ps
  Left _ -> 0

partOne :: String -> Int
partOne = parseAndCountValid passportsP

-- Part Two --------------------------------------------------------------------

-- --------------
-- Helper parsers
-- --------------

-- | The `fieldEndP` parser performs lookahead for any space character or EOF.
--   It is used to scan until end of field, in case of parser failure, as well
--   as impose additional constraint on fixed-length parsers (like `yearP`).
fieldEndP :: Parser Char
fieldEndP = lookAhead $ try (space <|> (eof >> return '\n'))

-- | The `failP` parser consumes characters until next field identifier and
--   returns Nothing. Useful for scanning until next field in case of parser
--   failure.
failP :: Parser (Maybe PassportField)
failP = Nothing <$ manyTill anyChar fieldEndP

yearP :: Parser Int
yearP = read <$> count 4 digit <* fieldEndP

-- -------------
-- Field parsers
-- -------------
-- Validation failure is treated the same as parsing failure.

birthYearP :: Parser (Maybe PassportField)
birthYearP = string "byr:" *> (try (f <$> yearP) <|> failP)
  where
    f yr =
      if yr >= 1920 && yr <= 2002
        then Just BirthYear
        else Nothing

issueYearP :: Parser (Maybe PassportField)
issueYearP = string "iyr:" *> (try (f <$> yearP) <|> failP)
  where
    f yr =
      if yr >= 2010 && yr <= 2020
        then Just IssueYear
        else Nothing

expirationYearP :: Parser (Maybe PassportField)
expirationYearP = string "eyr:" *> (try (f <$> yearP) <|> failP)
  where
    f yr =
      if yr >= 2020 && yr <= 2030
        then Just ExpirationYear
        else Nothing

heightP :: Parser (Maybe PassportField)
heightP = string "hgt:" *> (try p <|> failP)
  where
    p = do
      height <- (read :: String -> Int) <$> many1 digit
      unit <- string "cm" <|> string "in"
      return $ g height unit
      where
        g h u
          | u == "cm" && h >= 150 && h <= 193 = Just Height
          | u == "in" && h >= 59 && h <= 76 = Just Height
          | otherwise = Nothing

hairColorP :: Parser (Maybe PassportField)
hairColorP = string "hcl:" *> (try p <|> failP)
  where
    p = Just HairColor <$ (char '#' *> count 6 hexDigit)

eyeColorP :: Parser (Maybe PassportField)
eyeColorP = string "ecl:" *> (try p <|> failP)
  where
    p =
      Just EyeColor
        <$ choice
          [ string "amb",
            try $ string "blu",
            string "brn",
            try $ string "gry",
            string "grn",
            string "hzl",
            string "oth"
          ]

passportIdP :: Parser (Maybe PassportField)
passportIdP = string "pid:" *> (try p <|> failP)
  where
    p = Just PassportId <$ count 9 digit <* fieldEndP

countryIdP :: Parser (Maybe PassportField)
countryIdP = string "cid:" *> (try p <|> failP)
  where
    p = Just CountryId <$ many1 alphaNum

passportP' :: Parser [Maybe PassportField]
passportP' = fieldP' `sepEndBy` space
  where
    fieldP' =
      choice
        [ birthYearP,
          try expirationYearP,
          eyeColorP,
          issueYearP,
          try heightP,
          hairColorP,
          passportIdP,
          countryIdP
        ]

passportsP' :: Parser [Passport]
passportsP' = map catMaybes <$> passportP' `sepBy` newline <* eof

partTwo :: String -> Int
partTwo = parseAndCountValid passportsP'

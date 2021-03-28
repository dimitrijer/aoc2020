module Day02 where

import Text.Parsec (ParseError, eof, many, many1, parse, space)
import Text.Parsec.Char (anyChar, char, digit)
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)

-- Part One --------------------------------------------------------------------

data PasswordEntry = PasswordEntry
  { policyRange :: (Int, Int),
    policyChar :: Char,
    password :: String
  }
  deriving (Show, Eq)

countChar :: Char -> String -> Int
countChar c s = sum $ map (\x -> if x == c then 1 else 0) s

passwordEntryParser :: Parser PasswordEntry
passwordEntryParser = do
  l <- many1 digit <* char '-'
  h <- many1 digit <* space
  c <- anyChar <* char ':' <* space
  pw <- many anyChar <* eof
  case (readMaybe l :: Maybe Int, readMaybe h :: Maybe Int) of
    (Just low, Just high) ->
      return
        PasswordEntry
          { policyRange = (low, high),
            policyChar = c,
            password = pw
          }
    _ -> fail "failed to parse int values"

parsePasswordEntry :: String -> Either ParseError PasswordEntry
parsePasswordEntry = parse passwordEntryParser ""

isValid :: PasswordEntry -> Bool
isValid
  PasswordEntry
    { policyRange = (low, high),
      policyChar = c,
      password = p
    } = cnt >= low && cnt <= high
    where
      cnt = countChar c p

partOne :: String -> Int
partOne input = parseAndCountValidPasswords input isValid

-- Part Two --------------------------------------------------------------------

isValid' :: PasswordEntry -> Bool
isValid'
  PasswordEntry
    { policyRange = (low, high),
      policyChar = c,
      password = p
    } = case ( 0 <= idx1 && idx1 < length p && p !! idx1 == c,
               0 <= idx2 && idx2 < length p && p !! idx2 == c
             ) of
    (True, False) -> True
    (False, True) -> True
    _ -> False
    where
      idx1 = low - 1
      idx2 = high - 1

parseAndCountValidPasswords :: String -> (PasswordEntry -> Bool) -> Int
parseAndCountValidPasswords s p = case fmap (length . filter p) $ mapM parsePasswordEntry $ lines s of
  Left _ -> 0
  Right len -> len

partTwo :: String -> Int
partTwo input = parseAndCountValidPasswords input isValid'

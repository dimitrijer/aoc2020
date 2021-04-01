module Day06 where

import qualified Data.Set as S

-- Part One --------------------------------------------------------------------

mapGroups :: ([String] -> a) -> [String] -> [a]
mapGroups f ss =
  case dropWhile null ss of
    [] -> []
    ss' -> f grp : mapGroups f rst
      where
        (grp, rst) = break null ss'

partOne :: String -> Int
partOne = sum . map length . mapGroups (S.fromList . concat) . lines

-- Part Two --------------------------------------------------------------------

partTwo :: String -> Int
partTwo = sum . map length . mapGroups f . lines
  where
    f = foldr (S.intersection . S.fromList) fullSet
    fullSet = S.fromList ['a' .. 'z']

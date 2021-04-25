module Day13 where

import Control.Applicative (liftA2)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Text.Parsec

-- Part One --------------------------------------------------------------------

type BusId = Int

type Time = Int

type Schedule = (Time, [Maybe BusId])

parseS :: String -> Either ParseError Schedule
parseS = parse p ""
  where
    p = (,) <$> timeP <* newline <*> busP `sepBy` char ','
    timeP = read <$> many1 digit
    busP = Nothing <$ char 'x' <|> Just . read <$> many1 digit

earliest :: Schedule -> (Time, BusId)
earliest (t, bs) = minimum . map f $ catMaybes bs
  where
    f = \b ->
      let (q, r) = t `divMod` b
          diff = if r == 0 then 0 else (q + 1) * b - t
       in (diff, b)

partOne :: String -> Either ParseError Int
partOne = fmap (liftA2 (*) fst snd . earliest) . parseS

-- Part Two --------------------------------------------------------------------

type Delay = Int

toDelays :: [Maybe BusId] -> [(BusId, Delay)]
toDelays bs = [(b, i) | (Just b, i) <- zip bs [0 ..]]

-- | `solve` returns a list of solutions to set of equations given by first
-- argument, where each element of `bs`, namely `(b, d)` corresponds to
-- predicate x `mod` b == b - d.  According to Chinese remainder theorem, there
-- exists a unique solution in modulo `product $ map fst bs`, because all bus
-- IDs are primes, and, by definition, pairwise coprimes.
solve :: [(BusId, Delay)] -> [Time]
solve bs = [solution + i * modulo | i <- [0 ..]]
  where
    modulo = product $ map fst bs
    -- Brute force unique solution in `mod` (b1 * b2 * ... * bn).
    (bmax, dmax) = maximum bs
    solution = head $ [v | i <- [1 ..], let v = i * bmax - dmax, v <= modulo && satisfies v]
    -- Construct a predicate that returns `True` if input is a solution to the
    -- input set of equations.
    satisfies = f bs []
    f [] ps = \x -> all ($ x) ps
    f ((b, d) : xs) ps =
      let p x = x `mod` b == (b - d) `mod` b
       in f xs (p : ps)

findCommon :: [[Time]] -> Time
findCommon [] = 0
findCommon xss =
  let largest = maximum $ map head xss
      tails = map (dropWhile (< largest)) xss
      uniq = nub . map head $ tails
   in if length uniq == 1
        then largest
        else findCommon tails

splitOptimal :: [[(BusId, Delay)]] -> [(BusId, Delay)] -> [[(BusId, Delay)]]
splitOptimal result [] = result
splitOptimal [] (b : bs) = splitOptimal [[b]] bs
splitOptimal (r : rs) (b : bs) =
  if fst b * product (map fst r) > 1000000000
    then splitOptimal ([b] : r : rs) bs
    else splitOptimal ((b : r) : rs) bs

partTwo :: String -> Either ParseError Int
partTwo = fmap (findCommon . map solve . splitOptimal [] . toDelays . snd) . parseS

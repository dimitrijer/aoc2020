{-# LANGUAGE TupleSections #-}

module Day07 where

import Data.Bifunctor (first)
import Data.Either (rights)
import Data.List (find, nub, partition)
import Data.Maybe (isJust, mapMaybe)
import Data.Tuple (swap)
import Text.Parsec
import Text.Parsec.String

-- Part One --------------------------------------------------------------------

-------------
-- Parsing --
-------------
newtype BagId = BagId String
  deriving (Eq, Show)

data Bag = Bag
  { bagId :: BagId,
    contents :: [(BagId, Int)]
  }
  deriving (Eq, Show)

bagIdP :: Parser BagId
bagIdP = do
  (c1, c2) <- (,) <$> many1 letter <* space <*> many1 letter <* space
  _ <- try (string "bags") <|> string "bag"
  return $ BagId (c1 ++ " " ++ c2)

bagP :: Parser Bag
bagP = do
  b <- bagIdP <* string " contain "
  bs <- (emptyP <|> contentsP) <* char '.'
  return $ Bag b bs
  where
    emptyP = [] <$ string "no other bags"
    contentP = (,) . (read :: String -> Int) <$> many1 digit <* space <*> bagIdP
    contentsP = (swap <$> contentP) `sepBy` string ", "

parseBags :: String -> [Bag]
parseBags = rights . map (parse bagP "") . lines

-----------
-- Logic --
-----------
isEmpty :: Bag -> Bool
isEmpty b = null $ contents b

findById :: [Bag] -> BagId -> Maybe Bag
findById bags bid = find ((== bid) . bagId) bags

-- | The `reduce` function collapses contents of provided bag by summing counts
-- of bags that have the same ID in the contents list.
reduce :: Bag -> Bag
reduce (Bag bid c) = Bag {bagId = bid, contents = concatMap sumCnt bids}
  where
    bids = nub $ map fst c
    sumCnt = \bid' ->
      let total = sum $ map snd $ filter ((== bid') . fst) c
       in [(bid', total)]

-- | The `splode` function takes a list of all known bags, and a single bag, and
-- recursively replaces contents of the provided bag until all it contains are
-- empty bags, or non-empty bags that should not be sploded.
splode :: [Bag] -> (BagId -> Bool) -> Bag -> Bag
splode bags p (Bag bid c) =
  if null toExpand
    then Bag bid c
    else splode bags p (Bag bid (new ++ old))
  where
    c' = mapMaybe (\(bid', cnt) -> (,cnt) <$> findById bags bid') c
    (toExpand, toKeep) = partition (\(b, _) -> not (isEmpty b) && p (bagId b)) c'
    old = map (first bagId) toKeep
    new = concatMap (uncurry splodeBag) toExpand

-- | The `splodeBag` function takes a bag and a number of times it appears, and
-- returns sploaded contents of the bag.
splodeBag :: Bag -> Int -> [(BagId, Int)]
splodeBag Bag {contents = c} cnt = (fmap . fmap) (cnt *) c

contains :: BagId -> Bag -> Bool
contains bid (Bag _ c) = isJust $ find ((== bid) . fst) c

partOne :: String -> Int
partOne s = length $ filter (contains (BagId "shiny gold")) bs'
  where
    bs = rights $ map (parse bagP "") $ lines s
    bs' = map (reduce . splode bs (/= BagId "shiny gold")) bs

-- Part Two --------------------------------------------------------------------

-- | The `markBags` function adds a special marker bag to every non-empty bag,
-- for counting themselves while sploading.
markBags :: [Bag] -> [Bag]
markBags bs =
  let markerBagId = BagId "sparkly rainbow"
      markerBag = Bag {bagId = markerBagId, contents = []}
      maybeMark =
        ( \b ->
            if isEmpty b
              then b
              else Bag (bagId b) ((markerBagId, 1) : contents b)
        )
   in markerBag : map maybeMark bs

partTwo :: String -> Maybe Int
partTwo s = do
  let bs = rights $ map (parse bagP "") $ lines s
  b <- findById bs (BagId "shiny gold")
  return $ sum . map snd . contents $ reduce (splode (markBags bs) (const True) b)

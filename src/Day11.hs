{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day11 where

import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Maybe (catMaybes, mapMaybe)

-- Part One --------------------------------------------------------------------

data Pos = Empty | Occupied | Floor
  deriving (Eq)

instance Show Pos where
  show Empty = "L"
  show Occupied = "#"
  show Floor = "."

isOccupied :: Pos -> Bool
isOccupied Occupied = True
isOccupied _ = False

data Layout a = Layout
  { rows :: Int,
    cols :: Int,
    ps :: [[a]]
  }
  deriving (Eq)

instance Show a => Show (Layout a) where
  show Layout {ps} = intercalate "\n" $ map (concatMap show) ps

instance Foldable Layout where
  foldr f a Layout {ps} = foldr f a $ concat ps

-- | Reshapes an array of elements into a layout with `cols` columns.
reshape :: Int -> [a] -> Layout a
reshape cols ps = reshape' ps Layout {rows = 0, cols = cols, ps = []}
  where
    reshape' [] l = l
    reshape' xs Layout {rows = rows', ps = ps'} =
      reshape' (drop cols xs) Layout {rows = rows' + 1, cols = cols, ps = ps' ++ [take cols xs]}

parseL :: String -> Layout Pos
parseL s = Layout {rows = nr, cols = nc, ps = poss}
  where
    parseC = \case
      'L' -> Just Empty
      '#' -> Just Occupied
      '.' -> Just Floor
      _ -> Nothing
    poss = map (mapMaybe parseC) (lines s)
    nr = length poss
    nc = length $ head poss

-- | Returns a 3x3 layout of neighbours of Pos at `(x, y)` position
-- in provided layout, including original Pos.
neighbours :: Layout Pos -> (Int, Int) -> Layout (Maybe Pos)
neighbours l (x, y) = reshape 3 ps'
  where
    ps' = fmap getPosAt . step l (x, y) <$> allDirs
    getPosAt = \(i, j) -> ps l !! i !! j

-- | Given a layout `l` and a Pos evolution function `f`, the `evolve` function
-- will avance all Pos in given layout for one step.
evolve :: (Layout Pos -> (Int, Int) -> Pos) -> Layout Pos -> Layout Pos
evolve f l = reshape (cols l) . map (f l) $ idxs
  where
    idxs = [(i, j) | i <- [0 .. rows l - 1], j <- [0 .. cols l - 1]]

evolveP :: Layout Pos -> (Int, Int) -> Pos
evolveP l (x, y) =
  let ns = neighbours l (x, y)
   in case ps l !! x !! y of
        Empty ->
          if Just Occupied `notElem` ns
            then Occupied
            else Empty
        Occupied ->
          if (length . filter isOccupied . catMaybes . toList) ns >= 5 -- including Pos at (x, y)
            then Empty
            else Occupied
        Floor -> Floor

evolveToStable :: (Layout Pos -> (Int, Int) -> Pos) -> Layout Pos -> Layout Pos
evolveToStable f l = fst . head . dropWhile (uncurry (/=)) $ zip es (tail es)
  where
    es = iterate (evolve f) l

partOne :: String -> Int
partOne = length . filter isOccupied . toList . evolveToStable evolveP . parseL

-- Part Two --------------------------------------------------------------------

data DirH = West | East
  deriving (Eq)

data DirV = North | South
  deriving (Eq)

data Dir = Dir (Maybe DirH) (Maybe DirV)
  deriving (Eq)

allDirs :: [Dir]
allDirs =
  [ Dir (Just West) (Just North),
    Dir Nothing (Just North),
    Dir (Just East) (Just North),
    Dir (Just West) Nothing,
    Dir Nothing Nothing,
    Dir (Just East) Nothing,
    Dir (Just West) (Just South),
    Dir Nothing (Just South),
    Dir (Just East) (Just South)
  ]

stepH :: Layout Pos -> Int -> Maybe DirH -> Maybe Int
stepH Layout {cols} y d =
  case d of
    Nothing -> Just y
    Just West ->
      if y - 1 >= 0 && y - 1 < cols
        then Just (y - 1)
        else Nothing
    Just East ->
      if y + 1 >= 0 && y + 1 < cols
        then Just (y + 1)
        else Nothing

stepV :: Layout Pos -> Int -> Maybe DirV -> Maybe Int
stepV Layout {rows} x d =
  case d of
    Nothing -> Just x
    Just North ->
      if x - 1 >= 0 && x - 1 < rows
        then Just (x - 1)
        else Nothing
    Just South ->
      if x + 1 >= 0 && x + 1 < rows
        then Just (x + 1)
        else Nothing

step :: Layout Pos -> (Int, Int) -> Dir -> Maybe (Int, Int)
step l (x, y) (Dir h v) = (,) <$> stepV l x v <*> stepH l y h

countOccupied :: Layout Pos -> (Int, Int) -> Int
countOccupied l (x, y) =
  sum . map fromEnum . catMaybes $ [isDirOccupied l (x, y) d | d <- allDirs, d /= Dir Nothing Nothing]

isDirOccupied :: Layout Pos -> (Int, Int) -> Dir -> Maybe Bool
isDirOccupied l (x, y) d =
  do
    (x', y') <- step l (x, y) d
    case ps l !! x' !! y' of
      Occupied -> return True
      Empty -> return False
      Floor -> isDirOccupied l (x', y') d

evolveP' :: Layout Pos -> (Int, Int) -> Pos
evolveP' l (x, y) =
  let n = countOccupied l (x, y)
   in case ps l !! x !! y of
        Occupied -> if n >= 5 then Empty else Occupied
        Empty -> if n == 0 then Occupied else Empty
        Floor -> Floor

partTwo :: String -> Int
partTwo = length . filter isOccupied . toList . evolveToStable evolveP' . parseL

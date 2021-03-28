-- Alternative solution, based on Foldable. More concise, but I don't like it.

module Day03a where

-- Part One --------------------------------------------------------------------

data Grid a = Grid [[a]] (Int, Int)
  deriving (Eq, Show)

instance Foldable Grid where
  foldr f a (Grid xss (r, d)) =
    case map (drop r . cycle) $ drop d xss of
      [] -> a
      yss -> foldr f (f (head $ head yss) a) (Grid yss (r, d))

numTrees :: String -> Int -> Int -> Int
numTrees s r d = foldr f 0 g
  where
    f = \x a -> if x == '#' then a + 1 else a
    g = Grid (lines s) (r, d)

partOne :: String -> Int
partOne s = numTrees s 3 1

-- Part Two --------------------------------------------------------------------

partTwo :: String -> Int
partTwo s = product $ map f [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  where
    f = uncurry $ numTrees s

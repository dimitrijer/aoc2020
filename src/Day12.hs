module Day12 where

import qualified Control.Monad.State as S
import Text.Parsec
import Text.Parsec.String (Parser)

-- Part One --------------------------------------------------------------------

type Position = (Int, Int)

data Direction = N | S | E | W
  deriving (Eq, Show)

data Heading = L | R
  deriving (Eq, Show)

data Instruction = Move Direction Int | Rotate Heading Int | Forward Int
  deriving (Eq, Show)

type ST = S.State (Position, Direction)

move :: Direction -> Int -> ST Position
move d' n = do
  (p, d) <- S.get
  let p' = moveP d' n p
  _ <- S.put (p', d)
  return p'

forward :: Int -> ST Position
forward n = do
  (_, d) <- S.get
  move d n

rotate :: Heading -> Int -> ST Position
rotate h deg = do
  (p, d) <- S.get
  let result = dir2deg d + sign h * deg
      d' =
        if result < 0
          then result + 360
          else result `mod` 360
  _ <- S.put (p, deg2dir d')
  return p
  where
    deg2dir 90 = N
    deg2dir 270 = S
    deg2dir 180 = W
    deg2dir _ = E

    dir2deg N = 90
    dir2deg S = 270
    dir2deg W = 180
    dir2deg E = 0

sign :: Heading -> Int
sign L = 1
sign R = negate 1

exec :: Instruction -> ST Position
exec (Move d n) = move d n
exec (Rotate h n) = rotate h n
exec (Forward n) = forward n

instrP :: Parser Instruction
instrP = movP <|> rotP <|> fwdP
  where
    movP = Move <$> dirP <*> numP
    rotP = Rotate <$> (L <$ char 'L' <|> R <$ char 'R') <*> numP
    fwdP = Forward <$> (char 'F' *> numP)

    dirP = N <$ char 'N' <|> S <$ char 'S' <|> E <$ char 'E' <|> W <$ char 'W'
    numP = read <$> many1 digit

partOne :: String -> Either ParseError Int
partOne s = do
  is <- parse (instrP `sepEndBy` space) "" s
  let st = mapM exec is
      (x, y) = last $ S.evalState st ((0, 0), E)
  return $ abs x + abs y

-- Part Two --------------------------------------------------------------------

type ST' = S.State (Position, Position)

moveP :: Direction -> Int -> Position -> Position
moveP d n (x, y) = case d of
  N -> (x, y + n)
  S -> (x, y - n)
  E -> (x + n, y)
  W -> (x - n, y)

move' :: Direction -> Int -> ST' Position
move' d n = do
  (ps, pw) <- S.get
  _ <- S.put (ps, moveP d n pw)
  return ps

forward' :: Int -> ST' Position
forward' n = do
  ((xs, ys), (xw, yw)) <- S.get
  let ps' = (xs + n * xw, ys + n * yw)
  _ <- S.put (ps', (xw, yw))
  return ps'

rotate' :: Heading -> Int -> ST' Position
rotate' h n = do
  (ps, (xw, yw)) <- S.get
  let x = fromIntegral xw :: Double
      y = fromIntegral yw
      theta = atan2 y x + fromIntegral (sign h * n) * pi / 180
      r = sqrt (x ** 2 + y ** 2)
      xw' = round (r * cos theta)
      yw' = round (r * sin theta)
  _ <- S.put (ps, (xw', yw'))
  return ps

exec' :: Instruction -> ST' Position
exec' (Move d n) = move' d n
exec' (Rotate h n) = rotate' h n
exec' (Forward n) = forward' n

partTwo :: String -> Either ParseError Int
partTwo s = do
  is <- parse (instrP `sepEndBy` space) "" s
  let st = mapM exec' is
      (x, y) = last $ S.evalState st ((0, 0), (10, 1))
  return $ abs x + abs y

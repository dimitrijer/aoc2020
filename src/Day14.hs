module Day14 where

import qualified Control.Monad.State as S
import Data.Bits
import Data.Int
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String (Parser)

-- Part One --------------------------------------------------------------------

type Mask = Int64

-- Identity element w.r.t. binary AND.
andId :: Mask
andId = complement zeroBits

-- Identity element w.r.t. binary OR.
orId :: Mask
orId = zeroBits

type Addr = Int64

type Val = Int64

type Memory = M.Map Addr Val

-- Memory, AND mask, OR mask.
type ST = S.State (Memory, Mask, Mask)

write :: Addr -> Val -> ST Memory
write a x = do
  (mem, mAnd, mOr) <- S.get
  let x' = (x .&. mAnd) .|. mOr
      mem' = M.insert a x' mem
  _ <- S.put (mem', mAnd, mOr)
  return mem'

setMask :: Mask -> Mask -> ST Memory
setMask mAnd mOr = do
  (mem, _, _) <- S.get
  S.put (mem, mAnd, mOr)
  return mem

data Instr = SetMask Mask Mask | Write Addr Val
  deriving (Eq, Show)

execI :: Instr -> ST Memory
execI (SetMask mAnd mOr) = setMask mAnd mOr
execI (Write a x) = write a x

parseMask :: String -> (Mask, Mask)
parseMask m = foldr reduceMasks (andId, orId) $ zipWith toggleBit (reverse m) [0 ..]
  where
    toggleBit = \c i -> case c of
      '0' -> (complement $ bit i, orId) -- set i-th bit to 0 in AND mask
      '1' -> (andId, bit i) -- set i-th bit to 1 in OR mask
      _ -> (andId, orId)
    reduceMasks = \(mAnd', mOr') (mAnd, mOr) -> (mAnd' .&. mAnd, mOr' .|. mOr)

instrP :: (String -> (Mask, Mask)) -> Parser Instr
instrP mp = try maskP <|> writeP
  where
    maskP = uncurry SetMask <$> (string "mask = " *> (mp <$> count 36 bitP))
    bitP = char '1' <|> char '0' <|> char 'X'
    writeP = Write <$> (string "mem[" *> numP) <*> (string "] = " *> numP)
    numP = (read :: String -> Int64) <$> many1 digit

partOne :: String -> Either ParseError Int64
partOne s = do
  is <- parse (instrP parseMask `sepEndBy` space) "" s
  let st = mapM execI is
  return $ M.foldr (+) 0 . last . S.evalState st $ (M.empty, andId, orId)

-- Part Two --------------------------------------------------------------------

-- We will be using the same state monad, but with semantical difference in
-- what the first mask does. The first mask now becomes a floating bit mask,
-- with 1s denoting 'X' characters. The second mask retains the same binary OR
-- behaviour.

type ST' = ST

write' :: Addr -> Val -> ST' Memory
write' a x = do
  (mem, mFloat, mOr) <- S.get
  -- Create a sequence of `setBit` and `clearBit` transformations that correspond
  -- to combinations of 1s in float flag.
  let fss = sequence [[(`clearBit` i), (`setBit` i)] | i <- [0 .. 35], mFloat `testBit` i]
      -- Apply each unique list of transformations to OR-d address. Add identity
      -- transformation for address with no float bit changes.
      as = map (foldr ($) (a .|. mOr)) $ [id] : fss
      mem' = foldr (`M.insert` x) mem as
  _ <- S.put (mem', mFloat, mOr)
  return mem'

execI' :: Instr -> ST' Memory
execI' (Write a x) = write' a x
execI' i = execI i

parseMask' :: String -> (Mask, Mask)
parseMask' m = foldr reduceMasks (orId, orId) $ zipWith toggleBit (reverse m) [0 ..]
  where
    toggleBit = \c i -> case c of
      'X' -> (bit i, orId) -- set i-th bit to 1 in floating mask
      '1' -> (orId, bit i) -- set i-th bit to 1 in OR mask
      _ -> (orId, orId)
    reduceMasks = \(mFloat', mOr') (mFloat, mOr) -> (mFloat' .|. mFloat, mOr' .|. mOr)

partTwo :: String -> Either ParseError Int64
partTwo s = do
  is <- parse (instrP parseMask' `sepEndBy` space) "" s
  let st = mapM execI' is
  return $ M.foldr (+) 0 . last . S.evalState st $ (M.empty, orId, orId)

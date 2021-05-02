module Day14 where

import qualified Control.Monad.State as S
import Data.Bits
import Data.Int
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.String (Parser)

-- Part One --------------------------------------------------------------------

type Mask = Int64

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
parseMask m = foldr reduceMasks (complement zeroBits, zeroBits) $ zipWith toggleBit (reverse m) [0 ..]
  where
    toggleBit = \c i -> case c of
      '0' -> (bit i, zeroBits) -- set i-th bit to 1 in AND mask
      '1' -> (zeroBits, bit i) -- set i-th bit to 1 in OR mask
      _ -> (zeroBits, zeroBits)
    reduceMasks = \(mAnd', mOr') (mAnd, mOr) -> (complement mAnd' .&. mAnd, mOr' .|. mOr)

instrP :: Parser Instr
instrP = try maskP <|> writeP
  where
    maskP = uncurry SetMask <$> (string "mask = " *> (parseMask <$> count 36 bitP))
    bitP = char '1' <|> char '0' <|> char 'X'
    writeP = Write <$> (string "mem[" *> numP) <*> (string "] = " *> numP)
    numP = (read :: String -> Int64) <$> many1 digit

partOne :: String -> Either ParseError Int64
partOne s = do
  is <- parse (instrP `sepEndBy` space) "" s
  let st = mapM execI is
      s0 = (M.empty, complement zeroBits, zeroBits)
  return $ M.foldr (+) 0 . last . S.evalState st $ s0

-- Part Two --------------------------------------------------------------------

partTwo :: String -> Int
partTwo = const 0

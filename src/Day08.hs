module Day08 where

import qualified Control.Monad.State.Lazy as S
import Data.Either (rights)
import Data.List (findIndices)
import Text.Parsec
import Text.Parsec.String (Parser)

-- Part One --------------------------------------------------------------------

data Op = Acc | Jmp | Nop
  deriving (Eq, Show)

data Instr = Instr Op Int
  deriving (Eq, Show)

instrP :: Parser Instr
instrP = Instr <$> opP <* space <*> argP
  where
    opP = Acc <$ string "acc" <|> Jmp <$ string "jmp" <|> Nop <$ string "nop"
    argP = (*) <$> signP <*> (read <$> many1 digit)
    signP = (-1) <$ char '-' <|> 1 <$ char '+'

parseIs :: String -> [Instr]
parseIs = rights . map (parse instrP "") . lines

-- (instruction pointer, accumulator)
type Reg = (Int, Int)

executeI :: Instr -> S.State Reg ()
executeI (Instr op arg) = do
  (ip, acc) <- S.get
  case op of
    Acc -> S.put (ip + 1, acc + arg)
    Jmp -> S.put (ip + arg, acc)
    Nop -> S.put (ip + 1, acc)

-- | The `execute` function executes a list of instructions (program). It
-- returns a Left accumulator value when infinite loop is detected, and Right
-- accumulator value when program successfully terminates.
execute ::
  -- | The list of instructions
  [Instr] ->
  -- | IPs visited so far
  [Int] ->
  -- | The flip IP (Nop <-> Jmp)
  Int ->
  -- | The state monad
  S.State Reg (Either Int Int)
execute is ips flipIp = do
  (ip, acc) <- S.get

  let isHalt = ip >= length is || ip < 0
      isLoop = ip `elem` ips

  case (isHalt, isLoop) of
    (True, _) -> return $ Right acc
    (_, True) -> return $ Left acc
    _ -> executeI i >> execute is (ip : ips) flipIp
      where
        i = (if flipIp == ip then flipI else id) (is !! ip)

partOne :: String -> Int
partOne s = snd $ S.execState (execute is [] (-1)) (0, 0)
  where
    is = parseIs s

-- Part Two --------------------------------------------------------------------

isAcc :: Instr -> Bool
isAcc (Instr Acc _) = True
isAcc _ = False

flipI :: Instr -> Instr
flipI (Instr Nop arg) = Instr Jmp arg
flipI (Instr Jmp arg) = Instr Nop arg
flipI (Instr Acc arg) = Instr Acc arg

partTwo :: String -> Int
partTwo s = head . rights $ map (\idx -> S.evalState (execute is [] idx) (0, 0)) flipIdxs
  where
    is = parseIs s
    flipIdxs = findIndices (not . isAcc) is

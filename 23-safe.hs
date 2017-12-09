{-# LANGUAGE BangPatterns #-}

import Data.Char
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq

type Registers = (Int, Int, Int, Int)

data Register = A | B | C | D deriving (Show, Eq)
data From = Register Register | Value Int deriving Show
data Instruction = Cpy From From | Inc Register | Dec Register | Jnz From From | Tgl Register deriving Show

run :: Registers -> Seq.Seq Instruction -> Int -> Registers
run !registers !instructions pc
  | pc >= Seq.length instructions  = registers
  | isMultiply instructions pc     = run (multiply registers instructions pc) instructions (pc + 5)
  | isAdd instructions pc          = run (add registers instructions pc) instructions (pc + 3)
  | otherwise                      = run nextRegisters nextInstructions (pc + incPc)
  where (nextRegisters, nextInstructions, incPc) = interpret registers instructions pc (Seq.index instructions pc)

isAdd instructions pc = isAdd' next3
  where
    next3 = F.toList $ Seq.take 3 $ Seq.drop pc instructions
    isAdd' ([(Inc x), (Dec y), (Jnz (Register z) (Value (-2)))]) = y == z
    isAdd' _ = False

add registers instructions pc = added
  where
    [(Inc x), (Dec y)] = F.toList $ Seq.take 2 $ Seq.drop pc instructions
    count = get registers y
    orig = get registers x
    looped = set registers y 0
    added = set looped x (orig + count)

isMultiply instructions pc = isMultiply' next5
  where
    next5 = F.toList $ Seq.take 5 $ Seq.drop pc instructions
    isMultiply'
      [ (Inc x)
      , (Dec y)
      , (Jnz (Register z) (Value (-2)))
      , (Dec q)
      , (Jnz (Register w) (Value (-5)))
      ] = y == z && q == w
    isMultiply' _ = False

multiply registers instructions pc = multiplied
  where
    [(Inc x), (Dec y), _, (Dec z)] = F.toList $ Seq.take 4 $ Seq.drop pc instructions
    count = get registers y
    times = get registers z
    orig = get registers x
    looped = set registers y 0
    timed = set looped z 0
    multiplied = set timed x (orig + count * times)

interpret :: Registers -> Seq.Seq Instruction -> Int -> Instruction -> (Registers, Seq.Seq Instruction, Int)
interpret registers instructions _ (Cpy from (Value _)) = (registers, instructions, 1)
interpret registers instructions _ (Cpy from (Register r)) = (copy registers from r, instructions, 1)
interpret registers instructions _ (Inc register) = (inc registers register, instructions, 1)
interpret registers instructions _ (Dec register) = (dec registers register, instructions, 1)
interpret registers instructions pc (Jnz from steps)
  | x /= 0                       = (registers, instructions, y)
  | otherwise                    = (registers, instructions, 1)
  where x = getValue registers from
        y = getValue registers steps
interpret registers instructions pc (Tgl r)
  | pc + x < Seq.length instructions  = (registers, Seq.adjust toggle (pc + x) instructions, 1)
  | otherwise                         = (registers, instructions, 1)
    where x = get registers r


getValue registers (Register r) = get registers r
getValue _ (Value x) = x

toggle (Inc r) = Dec r
toggle (Dec r) = Inc r
toggle (Tgl r) = Inc r
toggle (Jnz f t) = Cpy f t
toggle (Cpy f t) = Jnz f t
copy registers (Value x) to = set registers to x
copy registers (Register from) to = set registers to (get registers from)
inc registers register = set registers register (get registers register + 1)
dec registers register = set registers register (get registers register - 1)

get (a, _, _, _) A = a
get (_, b, _, _) B = b
get (_, _, c, _) C = c
get (_, _, _, d) D = d

set (a, b, c, d) A value = (value, b, c, d)
set (a, b, c, d) B value = (a, value, c, d)
set (a, b, c, d) C value = (a, b, value, d)
set (a, b, c, d) D value = (a, b, c, value)

parseInstruction :: String -> Instruction
parseInstruction line
  | ins == "cpy" = Cpy (toFrom first) (toFrom second)
  | ins == "inc" = Inc (toRegister first)
  | ins == "dec" = Dec (toRegister first)
  | ins == "jnz" = Jnz (toFrom first) (toFrom second)
  | ins == "tgl" = Tgl (toRegister first)
  where
    ins = take 3 line
    (first, rest) = span (not . isSpace) $ drop 4 line
    second = takeWhile (not . isSpace) $ tail rest
    toRegister "a" = A
    toRegister "b" = B
    toRegister "c" = C
    toRegister "d" = D
    toRegister s = error s
    toFrom ('-' : cs) = Value (-(read cs))
    toFrom s
      | all isDigit s = Value (read s)
      | otherwise = Register (toRegister s)

solve = do
  sample <- readFile "23-sample.txt"
  let
    sampleInstructions = Seq.fromList $ map parseInstruction $ lines sample
    sampleResult = run (0, 0, 0, 0) sampleInstructions 0
  putStrLn $ show $ get sampleResult A
  input <- readFile "23-input.txt"
  let
    instructions = Seq.fromList $ map parseInstruction $ lines input
    result = run (7, 0, 0, 0) instructions 0
  putStrLn $ show $ get result A
  let
    result2 = run (12, 0, 0, 0) instructions 0
  putStrLn $ show $ get result2 A

main = solve

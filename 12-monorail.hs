import Data.Char

type Registers = (Int, Int, Int, Int)

data Register = A | B | C | D
data From = Register Register | Value Int
data Instruction = Cpy From Register | Inc Register | Dec Register | Jnz From Int

run :: Registers -> [Instruction] -> Int -> Registers
run registers instructions pc
  | pc >= length instructions  = registers
  | otherwise                 = run nextRegisters instructions (pc + incPc)
  where (nextRegisters, incPc) = interpret registers (instructions !! pc)

interpret :: Registers -> Instruction -> (Registers, Int)
interpret registers (Cpy from register) = (copy registers from register, 1)
interpret registers (Inc register) = (inc registers register, 1)
interpret registers (Dec register) = (dec registers register, 1)
interpret registers (Jnz (Register register) steps)
  | get registers register /= 0 = (registers, steps)
  | otherwise                   = (registers, 1)
interpret registers (Jnz (Value x) steps)
  | x /= 0    = (registers, steps)
  | otherwise = (registers, 1)

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
  | ins == "cpy" = Cpy (toFrom first) (toRegister second)
  | ins == "inc" = Inc (toRegister first)
  | ins == "dec" = Dec (toRegister first)
  | ins == "jnz" = Jnz (toFrom first) (read second)
  where
    ins = take 3 line
    (first, rest) = span (not . isSpace) $ drop 4 line
    second = takeWhile (not . isSpace) $ tail rest
    toRegister "a" = A
    toRegister "b" = B
    toRegister "c" = C
    toRegister "d" = D
    toRegister s = error s
    toFrom s
      | all isDigit s = Value (read s)
      | otherwise = Register (toRegister s)

solve = do
  sample <- readFile "12-sample.txt"
  let
    sampleInstructions = map parseInstruction $ lines sample
    sampleResult = run (0, 0, 0, 0) sampleInstructions 0
  putStrLn $ show $ get sampleResult A
  input <- readFile "12-input.txt"
  let
    instructions = map parseInstruction $ lines input
    result = run (0, 0, 0, 0) instructions 0
  putStrLn $ show $ get result A
  let
    result2 = run (0, 0, 1, 0) instructions 0
  putStrLn $ show $ get result2 A

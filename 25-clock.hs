import Data.Char
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq

type Registers = (Int, Int, Int, Int)

data Register = A | B | C | D deriving (Show, Eq)
data From = Register Register | Value Int deriving Show
data Instruction = Cpy From From | Inc Register | Dec Register | Jnz From From | Out From deriving Show

run :: Registers -> Seq.Seq Instruction -> Int -> String
run registers instructions pc
  | pc >= Seq.length instructions  = ""
  | isMultiply instructions pc     = run (multiply registers instructions pc) instructions (pc + 5)
  | isAdd instructions pc          = run (add registers instructions pc) instructions (pc + 3)
  | not $ null out                 = out ++ run nextRegisters instructions (pc + incPc)
  | otherwise                      = run nextRegisters instructions (pc + incPc)
  where (nextRegisters, out, incPc) = interpret registers (Seq.index instructions pc)

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

interpret :: Registers -> Instruction -> (Registers, String, Int)
interpret registers (Cpy from (Value _)) = (registers, "", 1)
interpret registers (Cpy from (Register r)) = (copy registers from r, "", 1)
interpret registers (Inc register) = (inc registers register, "", 1)
interpret registers (Dec register) = (dec registers register, "", 1)
interpret registers (Jnz from steps)
  | x /= 0                       = (registers, "", y)
  | otherwise                    = (registers, "", 1)
  where x = getValue registers from
        y = getValue registers steps
interpret registers (Out from) = (registers, show $ getValue registers from, 1)


getValue registers (Register r) = get registers r
getValue _ (Value x) = x

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
  | ins == "out" = Out (toFrom first)
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

findFreq instructions = map fst $ filter isClock $ map runWith [1..]
  where
    runWith n = (n, run (n, 0, 0, 0) instructions 0)
    isClock (n, ('0' : '1' : '0' : '1' : '0' : '1' : '0' : '1' : '0' : '1' : '0' : '1' : '0' : _)) = True
    isClock _ = False


solve = do
  input <- readFile "25-input.txt"
  let
    instructions = Seq.fromList $ map parseInstruction $ lines input
    result = findFreq instructions
  return $ head result

main = solve

import Data.Char

type Position = (Int, Int)
data Move = U | D | L | R deriving Show

numpad1 =
  [ "123"
  , "456"
  , "789"
  ]

numpad2 =
  [ "--1--"
  , "-234-"
  , "56789"
  , "-ABC-"
  , "--D--"
  ]

dx L = -1
dx R = 1
dx _ = 0
dy U = -1
dy D = 1
dy _ = 0

start1 = (1, 1)
start2 = (0, 2)

follow numpad from instructions = foldl (move numpad) from instructions

move numpad pos @ (x, y) i
  | isLegal newPos  = newPos
  | otherwise       = pos
  where
    newPos = (x + dx i, y + dy i)
    isLegal (x, y) =
          y >= 0 && y < length numpad
      &&  x >= 0 && x < length (numpad !! y)
      &&  isAlphaNum (numpad !! y !! x)

followAll numpad start instructions = tail $ scanl (follow numpad) start instructions

parseInstructions input = map toMoves $ lines input
  where toMoves = map toMove
        toMove 'U' = U
        toMove 'D' = D
        toMove 'L' = L
        toMove 'R' = R

toNumber numpad (x, y) = numpad !! y !! x

solve1 = do
  input <- readFile "02-input.txt"
  return $ map (toNumber numpad1) $ followAll numpad1 start1 $ parseInstructions input

solve2 = do
  input <- readFile "02-input.txt"
  return $ map (toNumber numpad2) $ followAll numpad2 start2 $ parseInstructions input

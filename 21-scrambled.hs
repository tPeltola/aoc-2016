import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Sequence ((><))
import Text.Regex.Posix
import Data.List

data Value = Position Int | Letter Char

data Operation =
    Swap Value Value
  | Rotate Value
  | Reverse Int Int
  | Move Int Int

operate s (Swap (Position x) (Position y)) = Seq.update y xc $ Seq.update x yc s
  where
    xc = Seq.index s x
    yc = Seq.index s y
operate s (Swap (Letter x) (Letter y)) = fmap swap s
  where
    swap c
      | c == x    = y
      | c == y    = x
      | otherwise = c
operate s (Rotate (Position x))
  | x < 0 = toFront >< toBack
  | otherwise  = operate s $ Rotate $ Position (-(Seq.length s - x))
  where
    (toBack, toFront) = Seq.splitAt position s
    position = (abs x) `mod` (Seq.length s)
operate s (Rotate (Letter x)) = operate s $ Rotate $ Position times
  where
    index = fromJust $ Seq.elemIndexL x s
    times = 1 + index + (if index >= 4 then 1 else 0)
operate s (Reverse x y) = before >< (Seq.reverse toReverse) >< after
  where
    (before, beforeRest) = Seq.splitAt x s
    (toReverse, after) = Seq.splitAt (y - x + 1) beforeRest
operate s (Move x y) = Seq.insertAt y (Seq.index s x) $ Seq.deleteAt x s

operateAll s operations = foldl operate s operations

solveSample = do
  input <- readFile "21-sample.txt"
  let
    ops = map parseOperation $ lines input
  return $ operateAll (Seq.fromList "abcde") ops


type Match = (String, String, String, [String])

parseOperation :: String -> Operation
parseOperation line
  | isJust swapPos = let (_, _, _, [x, y]) = fromJust swapPos in Swap (Position $ read x) (Position $ read y)
  | isJust swapLetter = let (_, _, _, [x, y]) = fromJust swapLetter in Swap (Letter $ head x) (Letter $ head y)
  | isJust rotate = let (_, _, _, [dir, x]) = fromJust rotate in Rotate $ Position ((if dir == "left" then (-1) else 1) * read x)
  | isJust rotateLetter = let (_, _, _, [x]) = fromJust rotateLetter in Rotate $ Letter $ head x
  | isJust inverse = let (_, _, _, [x, y]) = fromJust inverse in Reverse (read x) (read y)
  | isJust move = let (_, _, _, [x, y]) = fromJust move in Move (read x) (read y)
  | otherwise = error line
  where
    swapPos = line =~~ "swap position ([0-9]+) with position ([0-9]+)" :: Maybe Match
    swapLetter = line =~~ "swap letter ([a-z]) with letter ([a-z])" :: Maybe Match
    rotate = line =~~ "rotate (left|right) ([0-9]+) step" :: Maybe Match
    rotateLetter = line =~~ "rotate based on position of letter ([a-z])" :: Maybe Match
    inverse = line =~~ "reverse positions ([0-9]+) through ([0-9]+)" :: Maybe Match
    move = line =~~ "move position ([0-9]+) to position ([0-9]+)" :: Maybe Match


solve = do
  input <- readFile "21-input.txt"
  let
    ops = map parseOperation $ lines input
  return $ operateAll (Seq.fromList "abcdefgh") ops
  return $ find (\ p -> (operateAll (Seq.fromList p) ops) == (Seq.fromList "fbgdceah")) $ permutations "abcdefgh"

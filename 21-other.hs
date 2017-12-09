import Control.Lens (_2, over)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Sequence ((><), Seq)
import qualified Data.Sequence as S
import Text.Megaparsec ((<|>), anyChar, char, optional, parseMaybe, string)
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.String (Parser)

data Dir = L | R deriving (Show)
data Action = Swap Int Int | SwapC Char Char | Rotate Dir Int | RotateC Char | Reverse Int Int
            | Move Int Int deriving (Show)

parser :: Parser Action
parser = parseSwap <|> parseSwapC <|> parseRotateR <|> parseRotateL
         <|> parseRotateC <|> parseReverse <|> parseMove
    where int = fromInteger <$> integer
          parseSwap = Swap <$> (string "swap position " *> int) <*> (string " with position " *> int)
          parseSwapC = SwapC <$> (string "swap letter " *> anyChar) <*> (string " with letter " *> anyChar)
          parseRotateR = Rotate R <$> (string "rotate right " *> int <* string " step" <* optional (char 's'))
          parseRotateL = Rotate L <$> (string "rotate left " *> int <* string " step" <* optional (char 's'))
          parseRotateC = RotateC <$> (string "rotate based on position of letter " *> anyChar)
          parseReverse = Reverse <$> (string "reverse positions " *> int) <*> (string " through " *> int)
          parseMove = Move <$> (string "move position " *> int) <*> (string " to position " *> int)

(!) = S.index

eval :: Seq Char -> Action -> Seq Char
eval s (Swap a b) = S.update b (s ! a) (S.update a (s ! b) s)
eval s (SwapC a b) = eval s (Swap a' b')
    where Just a' = S.elemIndexL a s
          Just b' = S.elemIndexL b s
eval s (Rotate L n) = b >< a
    where (a, b) = S.splitAt (n `mod` length s) s
eval s (Rotate R n) = eval s $ Rotate L (length s - n)
eval s (RotateC c) = eval s $ Rotate R i
    where Just i = (\x -> if x >= 4 then x+2 else x+1) <$> S.elemIndexL c s
eval s (Reverse x y) = a >< S.reverse b >< c
    where (a, (b, c)) = over _2 (S.splitAt (y-x+1)) $ S.splitAt x s
eval s (Move a b) = S.insertAt b (s ! a) (S.deleteAt a s)


part1 = scanl eval (S.fromList "abcdefgh") . mapMaybe (parseMaybe parser) . lines

solve1 = do
  input <- readFile "21-input.txt"
  return $ part1 input

eval' :: Seq Char -> Action -> Seq Char
eval' s (Rotate L n) = eval s (Rotate R n)
eval' s (Rotate R n) = eval s (Rotate L n)
eval' s (RotateC c) = go s 0
    where go s n
              | i == n = s
              | otherwise = go (eval s (Rotate L 1)) (n+1)
              where Just i = (\x -> if x >= 4 then x+2 else x+1) <$> S.elemIndexL c s
eval' s (Move a b) = eval s (Move b a)
eval' s x = eval s x

part2 :: String -> String
part2 = toList . foldl' eval' (S.fromList "fbgdceah") . mapMaybe (parseMaybe parser) . reverse . lines

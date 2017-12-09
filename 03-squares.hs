import Data.List
import Text.Regex.Posix

isPossibleTriangle sides = all isSmaller sides
  where
    isSmaller a = a < (sum $ delete a sides)

parseTriangles str = map toTriangle $ lines str
  where toTriangle :: String -> [Int]
        toTriangle line = map toInt (line =~ "[0-9]+" :: [[String]])
        toInt = read . head

solve1 = do
  input <- readFile "03-input.txt"
  return $ length $ filter isPossibleTriangle $ parseTriangles input

transposeEveryThird [] = []
transposeEveryThird (a : b : c : rest) = (transpose [a, b, c]) ++ (transposeEveryThird rest)

solve2 = do
  input <- readFile "03-input.txt"
  return $ length $ filter isPossibleTriangle $ transposeEveryThird $ parseTriangles input

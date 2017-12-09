import Data.Ord
import Data.Char
import Data.List
import qualified Data.Sequence as Seq

findMin blacklist = findMin' (Seq.unstableSortBy (comparing fst) blacklist) 0 0
findMin' blacklist i x
  | min > x   = x
  | max < x   = findMin' blacklist (i + 1) x
  | otherwise = findMin' blacklist (i + 1) (max + 1)
  where
    (min, max) = Seq.index blacklist i

findAll blacklist = findAll' (Seq.unstableSortBy (comparing fst) blacklist) 0 0 []
findAll' blacklist i x acc
  | Seq.length blacklist  == i = acc
  | min > x         = findAll' blacklist (i + 1) (max + 1) ([x..(min - 1)] : acc)
  | max < x         = findAll' blacklist (i + 1) x acc
  | otherwise       = findAll' blacklist (i + 1) (max + 1) acc
  where
    (min, max) = Seq.index blacklist i

parseRange :: String -> (Integer, Integer)
parseRange line = (read start, read $ tail end)
  where
    (start, end) = span isDigit line

solve = do
  input <- readFile "20-input.txt"
  let
    blacklist = Seq.fromList $ map parseRange $ lines input
  putStrLn $ show $ findMin blacklist
  putStrLn $ show $ sum $ map (length) $ findAll blacklist

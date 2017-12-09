import Data.Ord
import Data.List

counts str = map withCount letters
  where
    letters = nub str
    withCount c = (c, length $ elemIndices c str)

mostFrequent = fst . maximumBy (comparing snd) . counts

leastFrequent = fst . minimumBy (comparing snd) . counts

solve1 = do
  input <- readFile "06-input.txt"
  return $ map mostFrequent $ transpose $ lines input

solve2 = do
  input <- readFile "06-input.txt"
  return $ map leastFrequent $ transpose $ lines input

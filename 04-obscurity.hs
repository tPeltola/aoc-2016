import Data.Char
import Data.List
import Text.Regex.Posix

data Room = Room
  { name     :: String
  , sectorId :: Int
  , checksum :: String
  }
  deriving Show

isReal (Room name _ check) = check == (calculateCheck name)

decrypt (Room name id check) = Room (map rotate name) id check
  where rotate '-' = ' '
        rotate c = chr (((ord c) - (ord 'a') + id) `mod` 26 + (ord 'a'))

calculateCheck letters = map head $ take 5 $ sortBy strLength $ group $ sort $ filter isAlpha letters
  where strLength a b
          | length a > length b = LT
          | length a < length b = GT
          | otherwise           = EQ

parseRoom :: String -> Room
parseRoom str = Room name (read id) check
  where
    parts = (str =~ "([-a-z]+)-([0-9]+)\\[([a-z]+)\\]" :: (String, String, String, [String]))
    (_, _, _, [name, id, check]) = parts
solve1 = do
  input <- readFile "04-input.txt"
  return $ sum $ map sectorId $ filter isReal $ map parseRoom $ lines input

solve2 = do
  input <- readFile "04-input.txt"
  return $ map sectorId $ filter (isSubsequenceOf "northpole" . name) $ map decrypt $ filter isReal $ map parseRoom $ lines input

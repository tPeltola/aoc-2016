import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

paths :: Seq (Seq Char) -> [(Char, [(Char, Int)])]
paths terrain = map doSearch numbers
  where
    numbers = F.toList (terrain >>= (Seq.filter isDigit))
    doSearch n = (n, filter ((/=) n . fst) $ search [(pos n, [])] (Set.singleton $ pos n) terrain [])
    pos n =
      let
        (Just y) = Seq.findIndexL (isJust . Seq.elemIndexL n) terrain
        (Just x) = Seq.elemIndexL n $ Seq.index terrain y
        in (x, y)

search :: [((Int, Int), [(Int, Int)])] ->  Set (Int, Int) -> Seq (Seq Char) -> [(Char, Int)]  -> [(Char, Int)]
search [] _ _ acc = acc
search ((current @ (x, y), path) : rest) visited terrain acc =
  search (rest ++ newWithHistory) (Set.union visited $ Set.fromList new) terrain withCurrent
  where
    moves = [(1, 0), (-1, 0), (0, 1), (0, -1)]
    positions = map (\ (dx, dy) -> (x + dx, y + dy)) moves
    new = filter isNew $ filter isLegal positions
    isLegal (x', y') =
         y' >= 0
      && (y' < Seq.length terrain)
      && x' >= 0
      && (x' < Seq.length (Seq.index terrain y'))
      && ('#' /= Seq.index (Seq.index terrain y') x')
    isNew p = Set.notMember p visited
    newWithHistory = map (\ p -> (p, current : path)) new
    withCurrent
      | isDigit $ Seq.index (Seq.index terrain y) x = (Seq.index (Seq.index terrain y) x, length path) : acc
      | otherwise                                   = acc

tsm terrain = minimum $ map path perms
  where
    numbers = map fst ps
    otherNumbers = filter ((/=) '0') numbers
    perms = permutations otherNumbers
    ps = paths terrain
    path order = sum $ map distance $ zip ('0' : order) (order ++ ['0'])
    distance (n, n') = fromJust $ lookup n' $ fromJust $ lookup n ps

solve = do
  sample <- readFile "24-sample.txt"
  let
    sampleTerrain = Seq.fromList $ map Seq.fromList $ lines sample
  putStrLn $ show $ tsm sampleTerrain
  input <- readFile "24-input.txt"
  let
    terrain = Seq.fromList $ map Seq.fromList $ lines input
  return $ tsm terrain

import Data.Bits
import Data.Sequence ((><))
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

input :: Int
input = 1350
--input = 10

isOpen (x, y) = ones `mod` 2 == 0
  where
    q = x*x + 3*x + 2*x*y + y + y*y
    bits = q + input
    range = finiteBitSize bits - countLeadingZeros bits
    ones = length $ filter (testBit bits) [0 .. (range - 1)]

moves =
  [ (1, 0)
  , (-1, 0)
  , (0, 1)
  , (0, -1)
  ]

possibleMoves (x, y) = filter isOpen $ filter nonNegative moved
  where
    moved = map add moves
    add (dx, dy) = (x + dx, y + dy)
    nonNegative (x1, y1) = x1 >= 0 && y1 >= 0

search :: Seq.Seq ((Int, Int), [(Int, Int)]) -> (Int, Int) -> Set.Set (Int, Int) -> [(Int, Int)]
search locations goal visited
  | from == goal = history
  | otherwise    = search (rest >< Seq.fromList movesWithHistory) goal (Set.union visited $ Set.fromList newMoves)
  where
    (fromSeq, rest) = Seq.splitAt 1 locations
    (from, history) = Seq.index fromSeq 0
    newMoves = filter isNew $ possibleMoves from
    movesWithHistory = map (\ m -> (m, from : history)) newMoves
    isNew loc = Set.notMember loc visited

solve = length $ search (Seq.singleton ((1, 1), [])) (31, 39) Set.empty

search2 :: Seq.Seq ((Int, Int), [(Int, Int)]) -> (Int, Int) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
search2 locations goal visited
  | length history >= 50 = visited
  | otherwise            = search2 (rest >< Seq.fromList movesWithHistory) goal (Set.union visited $ Set.fromList newMoves)
  where
    (fromSeq, rest) = Seq.splitAt 1 locations
    (from, history) = Seq.index fromSeq 0
    newMoves = filter isNew $ possibleMoves from
    movesWithHistory = map (\ m -> (m, from : history)) newMoves
    isNew loc = Set.notMember loc visited

solve2 = Set.size $ search2 (Seq.singleton ((1, 1), [])) (100, 100) Set.empty

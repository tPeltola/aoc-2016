import Data.Ord
import Data.List
import Data.Foldable
import Text.Regex.Posix
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

data Node = Node
  { x :: Int
  , y :: Int
  , size :: Int
  , used :: Int
  , avail :: Int
  } deriving (Show, Eq)

data Grid = Grid
  { nodes :: Seq.Seq Node
  , goal :: (Int, Int)
  , dimensions :: (Int, Int)
  , viable :: Set.Set (Int, Int)
  }

viablePairs nodes = (Seq.filter ((<) 0 . used) byUsed) >>= pairsForNode
  where
    byUsed = Seq.sortBy (comparing used) nodes
    byAvail = Seq.sortBy (comparing (Down . avail)) nodes
    pairsForNode n = fmap (\n' -> (n, n')) $ Seq.filter (n /=) $ Seq.takeWhileL ((<) (used n) . avail) byAvail

parseNode :: String -> Node
parseNode line = Node (read x) (read y) (read s) (read u) (read a)
  where
    (_, _, _, [x, y, s, u, a]) = line =~ "/dev/grid/node-x([0-9]+)-y([0-9]+) +([0-9]+)T +([0-9]+)T +([0-9]+)T +[0-9]+%" :: (String, String, String, [String])

search ((current @ (x, y), path) : rest) visited grid @ (Grid nodes (goalX, goalY) (maxX, maxY) viable)
  | x == goalX && y == goalY = path
  | otherwise                = search (rest ++ newNodesWithHistory) (Set.union visited (Set.fromList newNodes)) grid
  where
    newNodesWithHistory = map (\ n -> (n, current : path) ) newNodes
    directions = [(0, 1), (0, -1), (1, 0), (-1, 0)]
    newNodes = filter isUnvisited $ filter isViable $ filter isInside $ map (\ (dx, dy) -> ((x + dx), (y + dy)) ) directions
    isInside (x', y') = x' >= 0 && x' <= maxX && y' >= 0 && y' <= maxY
    isViable n = Set.member n viable
    isUnvisited n = Set.notMember n visited

solve = do
  input <- readFile "22-input.txt"
  let
    nodes = fmap parseNode $ Seq.fromList $ drop 2 $ lines input
    viables = viablePairs nodes
  putStrLn $ show $ Seq.length $ viables
  let
    maxX = maximum $ toList $ fmap x nodes
    maxY = maximum $ toList $ fmap y nodes
    dims = (maxX, maxY)
    goal = (maxX, 0)
    viable = Set.fromList $ toList $ fmap (\ n -> (x n, y n)) $ fmap fst viables
    grid = Grid nodes goal dims viable
    (Node startX startY _ _ _) = snd $ Seq.index viables 0
    start = (startX, startY)
    path = search [(start, [])] (Set.singleton start) grid
  return $ (length path + (5 * (maxX - 1)))

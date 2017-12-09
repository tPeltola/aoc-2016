import Text.Regex.Posix
import qualified Data.Set as Set

data Direction = N | S | E | W deriving Show
data Turn = L | R deriving Show
data Position = Position
  { x         :: Int
  , y         :: Int
  , direction :: Direction
  }
  deriving Show

turn N L = W
turn N R = E
turn S L = E
turn S R = W
turn W L = S
turn W R = N
turn E L = N
turn E R = S

walk N moves = (0, moves)
walk S moves = (0, -moves)
walk W moves = (-moves, 0)
walk E moves = (moves, 0)

start = Position 0 0 N

distance (Position x y _) = (abs x) + (abs y)

parseMoves :: String -> [(Turn, Int)]
parseMoves str = map toMove matches
  where matches = str =~ "(L|R)([0-9]+)" :: [[String]]
        toMove [_, "L", moves] = (L, read moves)
        toMove [_, "R", moves] = (R, read moves)

solve1 = do
  input <- readFile "01-input.txt"
  return $ distance $ fst $ follow start $ parseMoves input

follow from moves = foldl moveOneByOne (from, [from]) moves

moveOneByOne ((Position x y direction), path) (turnDirection, moves) =
    (last newPath, (init path) ++ newPath)
  where newPath = scanl step (Position x y newDirection) ones
        newDirection = turn direction turnDirection
        (dx, dy) = walk newDirection moves

        ones = take count $ repeat (ddx, ddy)
        count = (abs dx) + (abs dy)
        ddx = signum dx
        ddy = signum dy

        step (Position x' y' d) (dx', dy') = Position (x' + dx') (y' + dy') d


firstTwice positions = firstTwice' positions Set.empty
firstTwice' (pos @ (Position x y _) : rest) visited
  | Set.member (x, y) visited = pos
  | otherwise                 = firstTwice' rest (Set.insert (x, y) visited)

solve2 = do
  input <- readFile "01-input.txt"
  return $ distance $ firstTwice $ snd $ follow start $ parseMoves input

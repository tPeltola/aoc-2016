import Data.Maybe
import Data.List
import Data.Char
import Text.Regex.Posix
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence ((><))

type Facility = [Floor]
data Floor = Floor
  { elevator :: Bool
  , generators :: Set.Set String
  , microchips :: Set.Set String
  } deriving (Show, Eq, Ord)

data Direction = Up | Down deriving (Show, Eq)
data Move = Move
  { direction :: Direction
  , movedGenerators :: Set.Set String
  , movedMicrochips :: Set.Set String
  } deriving Show

elevatorFloor facility = fromJust $ find elevator facility

isFinished :: Facility -> Bool
isFinished floors = all isEmpty $ init floors

isEmpty floor = (Set.null $ generators floor) && (Set.null $ microchips floor)

isFull move = (Set.size $ movedGenerators move) == 2
  || (Set.size $ movedMicrochips move) == 2
  || ((not $ Set.null $ movedMicrochips move) && (not $ Set.null $ movedGenerators move))

moves :: Facility -> [Move]
moves facility = concatMap combinations [Up, Down]
  where
    from = elevatorFloor facility
    combinations d = (powered d) ++ (chipCombinations d) ++ (generatorCombinations d)
    powered d = map (\n -> Move d (Set.singleton n) (Set.singleton n)) $ Set.toList $ Set.intersection (generators from) (microchips from)
    chipCombinations d = map (\chips -> Move d Set.empty (Set.fromList chips)) $ combis $ Set.toList $ microchips from
    generatorCombinations d = map (\gens -> Move d (Set.fromList gens) Set.empty) $ combis $ Set.toList $ generators from
    combis [] = []
    combis (s:ss) = ([s] : map (:[s]) ss) ++ (combis ss)

safeMoves :: Facility -> [Move] -> [Move]
safeMoves facility moves = preferOneDown $ filter shouldMoveDown $ filter fromIsSafe $ filter toIsSafe $ filter floorExists moves
  where
    from = elevatorFloor facility
    floorNumber = fromJust $ elemIndex from facility
    floorExists (Move Up _ _) = floorNumber + 1 < length facility
    floorExists (Move Down _ _) = floorNumber - 1 >= 0
    floorIsSafe gens chips = Set.null gens || Set.null chips || Set.null (chips `Set.difference` gens)
    fromIsSafe (Move _ gens chips) = floorIsSafe (generators from `Set.difference` gens) (microchips from `Set.difference` chips)
    toIsSafe (Move dir gens chips) = floorIsSafe (generators (to dir) `Set.union` gens) (microchips (to dir) `Set.union` chips)
    to Up = facility !! (floorNumber + 1)
    to Down = facility !! (floorNumber - 1)
    shouldMoveDown (Move Down _ _) = any (not . isEmpty) $ takeWhile (/= from) facility
    shouldMoveDown _ = True
    preferTwoUp moves
      | any isFull $ filter ((==) Up . direction) moves  = filter isFull $ filter ((==) Up . direction) moves ++ filter ((==) Down . direction) moves
      | otherwise         = moves
    preferOneDown moves
      | any (not . isFull) $ filter ((==) Down . direction) moves  = (filter ((==) Up . direction) moves) ++ (filter (not . isFull) $ filter ((==) Down . direction) moves)
      | otherwise         = moves

move :: Facility -> Move -> Facility
move facility (Move dir gens chips) = map update $ zip [0..] facility
  where
    update (idx, floor)
      | idx == floorNumber  = updatedFrom
      | idx == toNumber dir = updatedTo
      | otherwise           = floor
    from = elevatorFloor facility
    floorNumber = fromJust $ elemIndex from facility
    updatedFrom = Floor False (generators from `Set.difference` gens) (microchips from `Set.difference` chips)
    toNumber Up = (floorNumber + 1)
    toNumber Down = (floorNumber - 1)
    to dir = facility !! (toNumber dir)
    updatedTo = Floor True (generators (to dir) `Set.union` gens) (microchips (to dir) `Set.union` chips)

type History = [(Bool, Set.Set String, Set.Set String, Int)]
search :: Seq.Seq (Facility, [Move]) -> Set.Set History -> (Facility, [Move])
search states history
  | isFinished now  = state
  | otherwise       = search (rest >< (Seq.fromList newMoves)) (foldr Set.insert history $ map (historyHash . fst) newMoves)
  where
    (stateSeq, rest) = Seq.splitAt 1 states
    state = stateSeq `Seq.index` 0
    (now, steps) = state
    newMoves = filter isNew $ map toMove $ safeMoves now $ moves now
    toMove m = (move now m, m : steps)
    isNew (f, _) = Set.notMember (historyHash f) history
    historyHash f = map (\x ->
      ( elevator x
      , (microchips x) `Set.difference` (generators x)
      , (generators x) `Set.difference` (microchips x)
      , Set.size ((generators x) `Set.intersection` (microchips x))
      )
      ) f

toFinished :: Facility -> (Facility, [Move])
toFinished initial = search (Seq.singleton (initial, [])) Set.empty

parseFloor :: String -> Floor
parseFloor line = Floor False (Set.fromList gens) (Set.fromList chips)
  where
    stuff = map head (line =~ "([a-z]+(-compatible microchip| generator))" :: [[String]])
    gens = map (takeWhile isAlpha) $ filter (isSuffixOf "generator") stuff
    chips = map (takeWhile isAlpha) $ filter (isSuffixOf "microchip") stuff

solve = do
  input <- readFile "11-input.txt"
  let
    (first : rest) = map parseFloor $ lines input
    facility = first { elevator = True } : rest
    solution = toFinished facility
  putStrLn $ show facility
  putStrLn $ show $ snd solution
  putStrLn $ show $ length $ snd solution
  let
    newComponents = Set.fromList ["elerium", "dilithium"]
    facility2 = first
      { elevator = True
      , generators = newComponents `Set.union` (generators first)
      , microchips = newComponents `Set.union` (microchips first)
      } : rest
    solution2 = toFinished facility2
  putStrLn $ show $ length $ snd solution2

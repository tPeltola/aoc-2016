import qualified Data.Digest.Pure.MD5 as MD5
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.Char

passcode = "vwbaicqe"

data Direction = U | D | L | R deriving (Show, Eq)

directions = [U, D, L, R]

hash :: String -> String
hash s = show $ MD5.md5 $ BL.fromStrict $ B.pack s

search :: [((Int, Int), String)] -> [((Int, Int), String)]
search [] = []
search (current @ ((x, y), path) : rest) = current : search (rest ++ movesWithHistory)
  where
    movesWithHistory = map (\ (pos, d) -> (pos, path ++ (show d)) ) $ possibleMoves (x, y) path

move :: (Int, Int) -> Direction -> (Int, Int)
move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)
move (x, y) L = (x - 1, y)
move (x, y) R = (x + 1, y)

possibleMoves :: (Int, Int) -> String -> [((Int, Int), Direction)]
possibleMoves (x, y) path
  | isGoal ((x, y), path) = []
  | otherwise             = filter isOpen $ filter isInside $ zip (map (move (x, y)) directions) directions
  where
    isInside ((x', y'), _) = x' >= 0 && x' < 4 && y' >= 0 && y' < 4
    currentHash = hash (passcode ++ path)
    isOpen (_, U) = isOpenChar (currentHash !! 0)
    isOpen (_, D) = isOpenChar (currentHash !! 1)
    isOpen (_, L) = isOpenChar (currentHash !! 2)
    isOpen (_, R) = isOpenChar (currentHash !! 3)
    isOpenChar c = ord c > ord 'a'

isGoal ((x, y), _) = x == 3 && y == 3

solve1 = snd $ head $ filter isGoal $ search [((0, 0), "")]
solve2 = length $ snd $ last $ filter isGoal $ search [((0, 0), "")]

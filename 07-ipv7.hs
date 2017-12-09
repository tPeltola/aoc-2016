import Data.List
import Data.Maybe
import Text.Regex.Posix

type Address = ([String], [String])

hasTls (addr, hypernet) = any hasAbba addr && all (not . hasAbba) hypernet

hasAbba str
  | length str < 4 = False
hasAbba (a:(rest @ (b:c:d:_))) = (a == d && b == c && a /= b) || hasAbba rest

hasSsl (addr, hypernet) = any hasBab abas
  where
    abas = concatMap hasAba addr
    bab [a, b, _] = [b, a, b]
    hasBab aba = elem (bab aba) $ concatMap hasAba hypernet

hasAba str = map toStr $ filter isAba trios
  where
    trios = zip3 str (drop 1 str) (drop 2 str)
    isAba (a, b, c) = a == c && a /= b
    toStr (a, b, c) = [a, b, c]

parseAddress :: String -> Address
parseAddress line = (map (head . (parsed !!)) addrIndices, map (head . (parsed !!)) hypernetIndices)
  where
    parsed = line =~ "[a-z]+" :: [[String]]
    addrIndices = filter (< (length parsed)) $ take (length parsed) [0, 2..]
    hypernetIndices = filter (< (length parsed)) $ take (length parsed) [1, 3..]

solve1 = do
  input <- readFile "07-input.txt"
  return $ length $ filter hasTls $ map parseAddress $ lines input

solve2 = do
  input <- readFile "07-input.txt"
  return $ length $ filter hasSsl $ map parseAddress $ lines input

import Data.Vector
import qualified Data.List as L
import Prelude hiding (replicate, (++), drop, take, map, length, zip, sum, filter, foldl)
import Text.Regex.Posix
import Data.Maybe

type Screen = Vector (Vector Bool)

data Instruction = Rect Int Int | RotateCol Int Int | RotateRow Int Int

start = replicate 6 $ replicate 50 False

operate :: Screen -> Instruction -> Screen
operate screen (RotateCol x by) = map updateRow $ zip (generate (length screen) id) screen
  where
    col = map (! x) screen
    rotatedCol = rotate col by
    updateRow (idx, row) = update row (singleton (x, rotatedCol ! idx))
operate screen (RotateRow y by) = update screen (singleton (y, rotatedRow))
  where
    row = screen ! y
    rotatedRow = rotate row by
operate screen (Rect x y) = (map on $ take y screen) ++ (drop y screen)
  where
    on row = (replicate x True) ++ (drop x row)

rotate v by = (drop rotateBy v) ++ (take rotateBy v)
  where rotateBy = (length v) - by

pixelsLit screen = sum $ map (length . filter id) screen

type Match = (String,String,String,[String])

parseInstruction :: String -> Instruction
parseInstruction line
  | isJust rect = let (_, _, _, [x, y]) = fromJust rect in Rect (read x) (read y)
  | isJust row = let (_, _, _, [y, by]) = fromJust row in RotateRow (read y) (read by)
  | isJust col = let (_, _, _, [x, by]) = fromJust col in RotateCol (read x) (read by)
  | otherwise = error line
  where
    rect = line =~~ "rect ([0-9]+)x([0-9]+)" :: Maybe Match
    row = line =~~ "rotate row y=([0-9]+) by ([0-9]+)" :: Maybe Match
    col = line =~~ "rotate column x=([0-9]+) by ([0-9]+)" :: Maybe Match

interpret screen = L.intercalate "\n" $ toList $ map (toList . map toPixel) screen
  where
    toPixel True = '#'
    toPixel False = '.'

solve = do
  input <- readFile "08-input.txt"
  let end = foldl operate start $ map parseInstruction $ fromList $ lines input
  putStrLn $ interpret end
  return $ pixelsLit end

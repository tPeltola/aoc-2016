
traps :: [Bool] -> Int -> (Int, [Bool])
traps first rows = foldl generate (length $ filter not first, first) [0 .. (rows - 2)]

generate (count, previous) _ = (count + (length $ filter not current), current)
  where
    current = map isTrap [0 .. (length previous - 1)]
    isTrap i =
         ((trap (i - 1)) && (trap i) && (not (trap (i + 1))))
      || ((trap (i + 1)) && (trap i) && (not (trap (i - 1))))
      || ((trap (i - 1)) && (not (trap i)) && (not (trap (i + 1))))
      || ((trap (i + 1)) && (not (trap i)) && (not (trap (i - 1))))
    trap j
      | j < 0                = False
      | j >= length previous = False
      | otherwise            = previous !! j

parseRow line = map toTrap line
  where
    toTrap '.' = False
    toTrap '^' = True

solveSample = fst $ traps (parseRow ".^^.^.^^^^") 10

solve = do
  input <- readFile "18-input.txt"
  let
    first = parseRow $ head $ lines input
  putStrLn $ show $ fst $ traps first 40
  putStrLn $ show $ fst $ traps first 400000

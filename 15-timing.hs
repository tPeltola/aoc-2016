import Text.Regex.Posix

data Disc = Disc
  { number :: Int
  , positions :: Int
  , offset :: Int
  }

fallsThrough :: Int -> Disc -> Bool
fallsThrough time disc @ (Disc n positions offset) = (positionAt disc time) == ((positions - n) `mod` positions)

positionAt :: Disc -> Int -> Int
positionAt (Disc _ positions offset) time = (offset + time) `mod` positions

parseDisc :: String -> Disc
parseDisc line = Disc (read number) (read positions) (read offset)
  where
    (_, _, _, match) = (line =~ "Disc #([0-9]+) has ([0-9]+) positions; at time=0, it is at position ([0-9]+)." :: (String, String, String, [String]))
    [number, positions, offset] = match

solve = do
  input <- readFile "15-input.txt"
  let
    discs = map parseDisc $ lines input
  putStrLn $ show $ head $ filter (\ time -> all (fallsThrough time) discs) [0..]
  let
    discs2 = (Disc 7 11 0) : discs
  putStrLn $ show $ head $ filter (\ time -> all (fallsThrough time) discs2) [0..]

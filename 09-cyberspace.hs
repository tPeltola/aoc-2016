import Data.Char

decompress [] = []
decompress ('(' : repeated) = decompressed ++ decompress rest
  where
    (countDigits, afterCount) = span isDigit repeated
    count = read countDigits
    (timesDigits, afterTime) = span isDigit $ tail afterCount
    times = read timesDigits
    (toRepeat, rest) = splitAt count $ tail afterTime
    decompressed = take (times * count) $ cycle toRepeat
decompress (c : rest)
  | isSpace c = decompress rest
  | otherwise = c : decompress rest

lengthDeep [] = 0
lengthDeep ('(' : repeated) = decompressed + lengthDeep rest
  where
    (countDigits, afterCount) = span isDigit repeated
    count = read countDigits
    (timesDigits, afterTime) = span isDigit $ tail afterCount
    times = read timesDigits
    (toDeep, rest) = splitAt count $ tail afterTime
    toRepeat = lengthDeep toDeep
    decompressed = times * toRepeat
lengthDeep (c : rest)
  | isSpace c = 0
  | otherwise = 1 + lengthDeep rest

solve = do
  input <- readFile "09-input.txt"
  let decompressed = decompress input
  putStrLn $ show $ length decompressed
  let deep = lengthDeep input
  putStrLn $ show $ deep

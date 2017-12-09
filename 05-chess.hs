import Data.Digest.Pure.MD5
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Char

doorId = "reyedfim"

solve1 = map (!! 5) $ take 8 $ filter (isPrefixOf "00000") $ map (show . md5 . BL.fromStrict . B.pack) $ map (doorId ++) $ map show [0..]

solve2 = sequence $ map (fromHashes . intToDigit) [0..7]
  where
    hashes = filter (isPrefixOf "00000") $ map (show . md5 . BL.fromStrict . B.pack) $ map (doorId ++) $ map show [0..]
    withPosition hash = (hash !! 5, hash !! 6)
    withPositions = map withPosition hashes
    fromHashes i = lookup i withPositions

main = do
  putStrLn solve1
  return 0

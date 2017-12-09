import Data.Digest.Pure.MD5
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Char
import Data.Maybe

input = "yjdafjpo"

hashes = map md52017 $ map (input ++) $ map show [0..]
md52017 s = foldl (\bs _ -> show $ md5 $ BL.fromStrict $ B.pack bs) s [0..2016]
solve = last $ take 64 $ keys [0..] hashes
keys :: [Int] -> [String] -> [(Int, String)]
keys (i : is) (hash : rest)
  | (isJust trio) && (any (isInfixOf quintet) $ take 1000 rest) = (i, hash) : keys is rest
  | otherwise          = keys is rest
  where
    trio = find isTrio $ zip3 hash (drop 1 hash) (drop 2 hash)
    isTrio (a, b, c) = a == b && b == c
    quintet = take 5 $ repeat $ char $ fromJust trio
    char (a, _, _) = a

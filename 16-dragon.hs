{-# LANGUAGE BangPatterns #-}

import qualified Data.Sequence as Seq
import Data.Foldable
import Control.Monad
import Data.List
import Data.Sequence ((><), (|>), (<|))

repeatDragonCurve :: Int -> Seq.Seq Char  -> Seq.Seq Char
repeatDragonCurve target !s
  | Seq.length s >= target = Seq.take target s
  | otherwise              = repeatDragonCurve target $! (dragonCurve s)

dragonCurve :: Seq.Seq Char -> Seq.Seq Char
dragonCurve !a = a >< (Seq.singleton '0') >< b
  where
    b = switch <$!> (Seq.reverse a)
    switch '1' = '0'
    switch '0' = '1'

repeatChecksum !s
  | odd $ length s = s
  | otherwise = repeatChecksum $! checksum s

checksum2 s
  | even $ Seq.length s = checksum2 start >< checksum2 end
  | otherwise = checksum s
  where
    (start, end) = Seq.splitAt ((Seq.length s) `div` 2) s

checksum s = checksum' s 0 Seq.empty
checksum' !s !i !acc
  | Seq.length s <= i = acc
  | otherwise         = checksum' s (i + 2) $! (acc |> checksumChar)
  where
    a = s `Seq.index` i
    b = s `Seq.index` (i + 1)
    checksumChar
      | a == b    = '1'
      | otherwise = '0'

solve1 = repeatChecksum $ repeatDragonCurve 272 (Seq.fromList "11101000110010100")
solve2 = repeatChecksum $ repeatDragonCurve 35651584 (Seq.fromList "11101000110010100")

main = do
  putStrLn $ show solve1
  let solution = solve2
  putStrLn $ show $ solution

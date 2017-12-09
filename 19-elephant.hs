import qualified Data.Sequence as Seq
import Data.Sequence ((><), (|>), (<|))


stealLeft elves
  | even $ Seq.length elves = takeEveryOther elves
  | otherwise           = Seq.drop 1 $ takeEveryOther elves

takeEveryOther l = takeEveryOther' l True Seq.empty
takeEveryOther' l shouldTake acc
  | Seq.null l = acc
  | shouldTake = takeEveryOther' (Seq.drop 1 l) False (acc |> (l `Seq.index` 0))
  | otherwise  = takeEveryOther' (Seq.drop 1 l) True acc

stealAcross elves = stealAcross' elves 0
stealAcross' elves i
  | i < length elves = stealAcross' (Seq.deleteAt from elves) next
  | otherwise        = elves
  where
    from = (i + ((length elves) `div` 2)) `mod` (length elves)
    next
      | from < i = i
      | otherwise = i + 1

stealUntil :: (Seq.Seq Int -> Seq.Seq Int) -> Seq.Seq Int -> Seq.Seq Int
stealUntil from elves
  | Seq.length elves > 1 = stealUntil from $ from elves
  | otherwise            = elves

solve1 = stealUntil stealLeft $ Seq.fromList [1..3018458]
solve2 = stealUntil stealAcross $ Seq.fromList [1..3018458]

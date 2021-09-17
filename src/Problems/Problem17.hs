-- Split a list into two parts; the length of the first part is given.
-- Do not use any predefined predicates.

module Problems.Problem17
  ( test,
    split,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 17" expected $ split nums k
  where
    nums :: [Int]
    nums = [1 .. 99]
    k = 5
    expected = ([1 .. k], [k + 1 .. 99])

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split xs@(x : rest) k
  | k > 0 = (x : fstPart, sndPart)
  | otherwise = ([], xs)
  where
    (fstPart, sndPart) = split rest $ k - 1

-- Rotate a list N places to the left.
-- Hint: Use the predefined functions length and (++).

module Problems.Problem19
  ( test,
    rotate,
  )
where

import MyUtils (assert)
import Problems.Problem17 (split)

test :: IO ()
test = do
  assert "Problem 19 Test 01" expected1 $ rotate nums k1
  assert "Problem 19 Test 02" expected2 $ rotate nums k2
  where
    nums = [1 .. 100] :: [Int]
    k1 = 30
    expected1 = [31 .. 100] ++ [1 .. 30]
    k2 = -1020
    expected2 = [81 .. 100] ++ [1 .. 80]

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs k = sndPart ++ fstPart
  where
    (fstPart, sndPart) = split xs $ k `mod` length xs

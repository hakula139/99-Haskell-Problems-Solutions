-- Extract a slice from a list.
-- Given two indices, i and k, the slice is the list containing the elements
-- between the i'th and k'th element of the original list (both limits
-- included). Start counting the elements with 1.

module Problems.Problem18
  ( test,
    slice,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 18" expected $ slice nums i j
  where
    nums :: [Int]
    nums = [1 .. 99]
    i = 30
    j = 40
    expected = [i .. j]

slice :: [a] -> Int -> Int -> [a]
slice xs i j = take (j - i + 1) $ drop (i - 1) xs

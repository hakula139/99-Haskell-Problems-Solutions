-- Replicate the elements of a list a given number of times.

module Problems.Problem15
  ( test,
    repli,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 15" expected $ repli str n
  where
    str = "abc"
    n = 3
    expected = "aaabbbccc"

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

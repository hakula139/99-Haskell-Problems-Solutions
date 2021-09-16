-- Eliminate consecutive duplicates of list elements.
-- If a list contains repeated elements they should be replaced with a single
-- copy of the element. The order of the elements should not be changed.

module Problems.Problem08
  ( test,
    compress,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 08" expected $ compress str
  where
    str = "aaaabccaadeeee"
    expected = "abcade"

compress :: Eq a => [a] -> [a]
compress (x : rest@(y : _))
  | x == y = compress rest
  | otherwise = x : compress rest
compress xs = xs

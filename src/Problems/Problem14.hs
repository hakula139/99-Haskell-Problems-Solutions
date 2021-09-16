-- Duplicate the elements of a list.

module Problems.Problem14
  ( test,
    dupli,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 14" expected $ dupli str
  where
    str = "abcde"
    expected = "aabbccddee"

dupli :: [a] -> [a]
dupli = concatMap (\x -> [x, x])

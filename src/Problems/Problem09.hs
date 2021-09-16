-- Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate
-- sublists.

module Problems.Problem09
  ( test,
    pack,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 09" expected $ pack str
  where
    str = "aaaabccaadeeee"
    expected = ["aaaa", "b", "cc", "aa", "d", "eeee"]

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x : xs)
  | x == head subHead = (x : subHead) : subTail
  | otherwise = [x] : sub
  where
    sub@(subHead : subTail) = pack xs

-- Modified run-length encoding.
-- Modify the result of problem P10 in such a way that if an element has no
-- duplicates it is simply copied into the result list. Only elements with
-- duplicates are transferred as (N E) lists.

module Problems.Problem11
  ( test,
    encodeModified,
  )
where

import MyUtils (assert)
import Problems.Problem09 (pack)

data Count a = Multiple Int a | Single a
  deriving (Eq, Show)

test :: IO ()
test = do
  assert "Problem 11" expected $ encodeModified str
  where
    str = "aaaabccaadeeee"
    expected =
      [ Multiple 4 'a',
        Single 'b',
        Multiple 2 'c',
        Multiple 2 'a',
        Single 'd',
        Multiple 4 'e'
      ]

segToCount :: Eq a => [a] -> Count a
segToCount seg
  | count == 1 = Single val
  | otherwise = Multiple count val
  where
    count = length seg
    val = head seg

encodeModified :: Eq a => [a] -> [Count a]
encodeModified xs = map segToCount $ pack xs

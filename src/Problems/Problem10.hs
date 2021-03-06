-- Run-length encoding of a list.
-- Use the result of problem P09 to implement the so-called run-length encoding
-- data compression method. Consecutive duplicates of elements are encoded as
-- lists (N E) where N is the number of duplicates of the element E.

module Problems.Problem10
  ( test,
    encode,
  )
where

import MyUtils (assert)
import Problems.Problem09 (pack)

test :: IO ()
test = do
  assert "Problem 10" expected $ encode str
  where
    str = "aaaabccaadeeee"
    expected =
      [ (4, 'a'),
        (1, 'b'),
        (2, 'c'),
        (2, 'a'),
        (1, 'd'),
        (4, 'e')
      ]

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\seg -> (length seg, head seg)) $ pack xs

-- Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly.
-- I.e. don't explicitly create the sublists containing the duplicates, as in
-- problem P9, but only count them. As in problem P11, simplify the result list
-- by replacing the singleton lists (1 X) by X.

module Problems.Problem13
  ( test,
    encodeDirect,
  )
where

import MyUtils (assert)

data Entry a = Multiple Int a | Single a
  deriving (Eq, Show)

test :: IO ()
test = do
  assert "Problem 13" encodedStr $ encodeDirect str
  where
    str = "aaaabccaadeeee"
    encodedStr =
      [ Multiple 4 'a',
        Single 'b',
        Multiple 2 'c',
        Multiple 2 'a',
        Single 'd',
        Multiple 4 'e'
      ]

packDirect :: Eq a => [a] -> [(Int, a)]
packDirect [] = []
packDirect [x] = [(1, x)]
packDirect (x : xs)
  | x == val = (count + 1, val) : subTail
  | otherwise = (1, x) : sub
  where
    sub@((count, val) : subTail) = packDirect xs

packToEntry :: Eq a => (Int, a) -> Entry a
packToEntry (count, val)
  | count == 1 = Single val
  | otherwise = Multiple count val

encodeDirect :: Eq a => [a] -> [Entry a]
encodeDirect xs = map packToEntry $ packDirect xs

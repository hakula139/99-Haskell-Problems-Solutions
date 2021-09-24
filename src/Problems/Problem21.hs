-- Insert an element at a given position into a list.

module Problems.Problem21
  ( test,
    insertAt,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 21" expected $ insertAt c str pos
  where
    c = 'X'
    str = "abcde"
    pos = 3
    expected = "abXcde"

insertAt :: a -> [a] -> Int -> [a]
insertAt e xs 1 = e : xs
insertAt e (x : xs) pos = x : insertAt e xs (pos - 1)
insertAt _ [] _ = error "index is out of bounds"

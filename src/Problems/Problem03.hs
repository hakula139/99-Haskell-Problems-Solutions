-- Find the K'th element of a list. The first element in the list is number 1.

module Problems.Problem03
  ( test,
    elementAt,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 03" kthNum $ elementAt nums k
  where
    nums = [1, 3 ..] :: [Integer]
    k = 30
    kthNum = 59

elementAt :: [a] -> Int -> a
elementAt [] _ = error "index is out of bounds"
elementAt (x : _) 1 = x
elementAt (_ : rest) k
  | k < 1 = error "index is out of bounds"
  | otherwise = elementAt rest $ k - 1

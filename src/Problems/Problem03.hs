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
    nums :: [Integer]
    nums = [1, 3 .. 999]
    k = 30
    kthNum = 59

elementAt :: [a] -> Int -> a
elementAt [] _ = error "empty list"
elementAt list@(x : rest) k
  | k <= 0 || k > size =
    error $
      "index " ++ show k ++ " is out of bounds [1, " ++ show size ++ "]"
  | k == 1 = x
  | otherwise = elementAt rest $ k - 1
  where
    size = length list

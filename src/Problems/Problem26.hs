-- Generate the combinations of K distinct objects chosen from the N elements
-- of a list.

module Problems.Problem26
  ( test,
    combinations,
  )
where

import Data.List (tails)
import MyUtils (assert)

test :: IO ()
test = do
  -- print result
  assert "Problem 26" expected $ fromIntegral $ length result
  where
    nums = [1 .. n] :: [Integer]
    k = 7
    n = 12
    result = combinations k nums
    expected = cnk n k

cnk :: Integral a => a -> a -> a
cnk n k
  | k < 0 || n < 0 || k > n = error "invalid argument"
  | k == 0 = 1
  | k > n `div` 2 = cnk n (n - k)
  | otherwise = product [n - k + 1 .. n] `div` product [1 .. k]

combinations :: Integral a => a -> [b] -> [[b]]
combinations k xs
  | k < 0 = error "invalid argument"
  | k == 0 = [[]]
  | otherwise =
    [ y : ys'
      | y : ys <- tails xs,
        ys' <- combinations (k - 1) ys
    ]

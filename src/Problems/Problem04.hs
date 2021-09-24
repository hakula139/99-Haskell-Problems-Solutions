-- Find the number of elements of a list.

module Problems.Problem04
  ( test,
    myLength,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 04" (length nums) (myLength nums)
  where
    nums = [1 .. 99] :: [Integer]

myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs

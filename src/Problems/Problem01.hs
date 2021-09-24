-- Find the last element of a list.

module Problems.Problem01
  ( test,
    myLast,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 01" lastNum (myLast nums)
  where
    nums = [0 .. lastNum] :: [Integer]
    lastNum = 999

myLast :: [a] -> a
myLast [] = error "empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

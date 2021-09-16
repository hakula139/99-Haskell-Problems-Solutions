-- Reverse a list.

module Problems.Problem05
  ( test,
    myReverse,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 05" (reverse str) (myReverse str)
  where
    str = "Hello, world!"

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

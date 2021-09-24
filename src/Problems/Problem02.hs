-- Find the last but one element of a list.

module Problems.Problem02
  ( test,
    myButLast,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 02" prevLastChar (myButLast chars)
  where
    chars = ['a' .. succ prevLastChar]
    prevLastChar = 'y'

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [_] = error "list size < 2"
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

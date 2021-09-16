-- Find out whether a list is a palindrome. A palindrome can be read forward or
-- backward; e.g. (x a m a x).

module Problems.Problem06
  ( test,
    isPalindrome,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 06 Test 01" False $ isPalindrome str1
  assert "Problem 06 Test 02" True $ isPalindrome str2
  where
    str1 = "Hello, world!"
    str2 = "Hello olleH"

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome xs =
  head xs == last xs && isPalindrome (init $ tail xs)

-- Create a list containing all integers within a given range.

module Problems.Problem22
  ( test,
    range,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 22" nums $ range begin end
  where
    begin = 10
    end = 99
    nums :: [Integer]
    nums = [begin .. end]

range :: Integral a => a -> a -> [a]
range begin end
  | begin == end = [end]
  | begin < end = begin : range (begin + 1) end
  | otherwise = reverse $ range end begin

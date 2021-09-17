-- Drop every N'th element from a list.

module Problems.Problem16
  ( test,
    dropEvery,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 16" expected $ dropEvery str k
  where
    str = "abcdefghijk"
    k = 3
    expected = "abdeghjk"

dropEvery :: [a] -> Int -> [a]
dropEvery xs k =
  [ x
    | (x, i) <- zip xs [1 ..],
      i `mod` k /= 0
  ]

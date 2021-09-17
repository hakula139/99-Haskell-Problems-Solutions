-- Remove the K'th element from a list.

module Problems.Problem20
  ( test,
    removeAt,
  )
where

import MyUtils (assert)

test :: IO ()
test = do
  assert "Problem 20 Test 01" expected1 $ removeAt k1 str
  assert "Problem 20 Test 02" expected2 $ removeAt k2 str
  where
    str = "abcde"
    k1 = 2
    expected1 = (Just 'b', "acde")
    k2 = 7
    expected2 = (Nothing, "abcde")

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt k xs = case sndPart of
  [] -> (Nothing, xs)
  x : rest -> (Just x, fstPart ++ rest)
  where
    (fstPart, sndPart) = splitAt (k - 1) xs

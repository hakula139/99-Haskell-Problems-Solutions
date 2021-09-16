-- Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem P11.
-- Construct its uncompressed version.

module Problems.Problem12
  ( test,
    decodeModified,
  )
where

import MyUtils (assert)

data Entry a = Multiple Int a | Single a
  deriving (Eq, Show)

test :: IO ()
test = do
  assert "Problem 12" str $ decodeModified encodedStr
  where
    str = "aaaabccaadeeee"
    encodedStr =
      [ Multiple 4 'a',
        Single 'b',
        Multiple 2 'c',
        Multiple 2 'a',
        Single 'd',
        Multiple 4 'e'
      ]

entryToSeg :: Eq a => Entry a -> [a]
entryToSeg (Multiple count val) = replicate count val
entryToSeg (Single val) = [val]

decodeModified :: Eq a => [Entry a] -> [a]
decodeModified = concatMap entryToSeg

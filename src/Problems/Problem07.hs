-- Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into a 'flat' list by
-- replacing each list with its elements (recursively).

module Problems.Problem07
  ( test,
    flatten,
  )
where

import MyUtils (assert)

-- We have to define a new data type, because lists in Haskell are homogeneous.
-- Our NestedList datatype is either a single element of some type (Elem a),
-- or a list of NestedLists of the same type. (List [NestedList a]).
data NestedList a = Elem a | List [NestedList a]

test :: IO ()
test = do
  assert "Problem 07 Test 01" expected1 $ flatten list1
  assert "Problem 07 Test 02" expected2 $ flatten list2
  where
    list1 :: NestedList Integer
    list1 = Elem 7
    expected1 = [7]

    list2 :: NestedList Integer
    list2 =
      List
        [ Elem 1,
          List [Elem 2, Elem 3],
          List [],
          List
            [ Elem 4,
              List [Elem 5, Elem 6]
            ],
          List [List []]
        ]
    expected2 = [1, 2, 3, 4, 5, 6]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
-- flatten (List xs) = concatMap flatten xs
flatten (List []) = []
flatten (List (x : xs)) = flatten x ++ flatten (List xs)

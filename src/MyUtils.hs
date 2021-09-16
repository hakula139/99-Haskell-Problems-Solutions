module MyUtils
  ( assert,
  )
where

assert :: (Eq a, Show a) => String -> a -> a -> IO ()
assert funcName expected got =
  if expected == got
    then
      putStrLn $
        funcName
          ++ " passed"
    else
      error $
        funcName
          ++ " assertion failed: expected "
          ++ show expected
          ++ " got "
          ++ show got

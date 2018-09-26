-- Unsafe API using non-total functions.
head :: [a] -> a
head xs = case xs of
  (x:_) -> x
  []    -> error "empty list!"

endpts = do
  putStrLn "Enter a non-empty list of integers:"
  xs <- readLn
  if xs /= [] then return (head xs, head (reverse xs))
              else endpts

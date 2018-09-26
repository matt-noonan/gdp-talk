module FancySafeMerge where

mergeMay :: (a -> a -> Ordering) -> [a] -> [a] -> Maybe [a]

mergeMay comp xs ys =
    if isSorted xs && isSorted ys
      then Just (mergeBy comp xs ys)
      else Nothing

  where
    isSorted (z : zs@(z' : _)) =  comp z z' /= GT
                               && isSorted zs
    isSorted _ = True

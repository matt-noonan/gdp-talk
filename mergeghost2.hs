-- This type reads as:
-- "mergeGDP takes a comparator and two lists that
-- have been sorted by that same comparator.
--
-- It returns a new list that is also sorted by that
-- same comparator."

mergeGDP :: ((a -> a -> Ordering) ~~ comp)
         -> SortedBy comp [a]
         -> SortedBy comp [a]
         -> SortedBy comp [a]

mergeGDP comp xs ys =
    coerce (mergeBy (the comp) (the xs) (the ys))

module GDP.Merge (SortedBy, mergeGDP, sortGDP) where
                  -- ^ constructor NOT exported!

-- A `SortedBy comp a` is an `a` that
-- has been sorted by `comp`.
newtype SortedBy comp a = SortedBy a

instance The (SortedBy comp a) a

-- How do we get a `SortedBy comp [a]`?
-- By sorting a list using a comparator named `comp`!
sortGDP :: ((a -> a -> Ordering) ~~ comp)
        -> [a]
        -> SortedBy comp [a]

sortGDP comp = coerce . sortBy (the comp)

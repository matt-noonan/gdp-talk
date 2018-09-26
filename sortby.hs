sortBy  :: (a -> a -> Ordering) -> [a]        -> [a]
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]

-- BE CAREFUL! xs and ys must already be sorted by comp!
mergeBy comp xs ys = case (xs, ys) of
    (_, []) -> xs
    ([], _) -> ys
    ((x:xs'), (y:ys')) -> case comp x y of
        LT -> x     : mergeBy comp xs' ys
        GT ->     y : mergeBy comp xs  ys'
        EQ -> x : y : mergeBy comp xs' ys'

import FancySafeMerge

merge3 :: (a -> a -> Ordering) -> [a] -> [a] -> [a] -> [a]
merge3 cmp xs ys zs =
    let xs' = sortBy   cmp xs
        ys' = sortBy   cmp ys
        zs' = sortBy   cmp zs
        yzs = fromJust (mergeMay cmp ys' zs')
    in fromJust (mergeMay cmp xs' yzs)

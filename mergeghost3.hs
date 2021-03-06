import GDP.Merge

myMergeDown :: [Int] -> [Int] -> [Int]
myMergeDown xs ys =
  name (comparing Down) $ \comp ->
    let xs' = sortGDP comp xs
        ys' = sortGDP comp ys

    in      the  (mergeGDP comp xs' ys')

import FancySafeMerge

myMergeDown :: [Int] -> [Int] -> [Int]
myMergeDown xs ys =
  let comp   = comparing Down  -- compare in reverse Ord-er
      xs'    = sortBy comp xs
      ys'    = sortBy comp ys

  in  fromJust (mergeMay comp xs' ys')

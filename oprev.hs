op :: ((a -> a -> Ordering) ~~ comp)
   -> ((a -> a -> Ordering) ~~ Op comp)

rev :: ([a] ~~ xs) -> ([a] ~~ Rev xs)
rev = defn . reverse . the

op_rev_axiom :: Proof (SortedBy (Op comp) (Rev xs))
             -> Proof (SortedBy comp xs)
op_rev_axiom = axiom

because :: (a ? p) -> (Proof (p a) -> Proof (q b)) -> (b ? q)

myMerge :: ([a] ? SortedBy comp)
        -> ([a] ? SortedBy (Op comp))
        -> ([a] ? SortedBy comp)
myMerge xs ys = mergeGDP xs (rev ys `because` op_rev_lemma)

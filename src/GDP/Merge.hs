{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module GDP.Merge (SortedBy, mergeGDP, sortGDP, op, rev, Op, Rev, op_rev_lemma) where

import           Data.Coerce
import           Data.List
import           Data.List.Utils (mergeBy)
import           GDP
import           Named
import           The

newtype SortedBy comp a = SortedBy a
instance The (SortedBy comp a) a

sortGDP :: ((a -> a -> Ordering) ~~ comp)
        -> [a]
        -> SortedBy comp [a]
sortGDP comp = coerce . sortBy (the comp)

mergeGDP :: ((a -> a -> Ordering) ~~ comp)
         -> SortedBy comp [a]
         -> SortedBy comp [a]
         -> SortedBy comp [a]
mergeGDP comp xs ys =
  coerce (mergeBy (the comp) (the xs) (the ys))

------------------------------------------------------

newtype Op comp = Op  Defn
newtype Rev xs  = Rev Defn

op :: ((a -> a -> Ordering) ~~ comp)
   -> ((a -> a -> Ordering) ~~ Op comp)
op comp = defn $ \x y -> case the comp x y of
  GT -> LT
  LT -> GT
  EQ -> EQ

rev :: ([a] ~~ xs) -> ([a] ~~ Rev xs)
rev = defn . reverse . the

op_rev_lemma :: Proof (SortedBy (Op comp) xs)
             -> Proof (SortedBy comp (Rev xs))
op_rev_lemma _ = axiom "The reverse of a list is sorted in the opposite order."


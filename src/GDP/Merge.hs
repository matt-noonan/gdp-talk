{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module GDP.Merge (SortedBy, mergeGDP, sortGDP
                 , mergeGDP', Merge, op, rev, Op, Rev, op_rev_lemma) where

import           Data.Coerce
import           Data.List
import           GDP
import           Named
import           The

mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy comp xs ys = case (xs,ys) of
    (_, []) -> xs
    ([], _) -> ys
    (x:xs', y:ys') -> case comp x y of
        LT -> x :     mergeBy comp xs' ys
        GT ->     y : mergeBy comp xs  ys'
        EQ -> x : y : mergeBy comp xs' ys'

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

newtype Op comp     = Op  Defn
newtype Rev xs      = Rev Defn
newtype Merge xs ys = Merge Defn

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

mergeGDP' :: ((a -> a -> Ordering) ~~ comp)
          -> (([a] ~~ xs) ::: SortedBy comp xs)
          -> (([a] ~~ ys) ::: SortedBy comp ys)
          -> ([a] ~~ Merge xs ys)
mergeGDP' comp xs ys =
  defn $ mergeBy (the comp) (the xs) (the ys)

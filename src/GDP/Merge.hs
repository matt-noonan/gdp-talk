{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module GDP.Merge (SortedBy, mergeGDP, sortGDP) where

import           Data.Coerce
import           Data.List
import           Data.List.Utils (mergeBy)
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


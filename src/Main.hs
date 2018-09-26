{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Data.Ord
import           GDP
import           GDP.Merge
import           Named
import           The

myMergeDown :: [Int] -> [Int] -> [Int]
myMergeDown xs ys = name (comparing Down) $ \comp ->
    let xs' = sortGDP comp xs
        ys' = sortGDP comp ys

    in the (mergeGDP comp xs' ys')

main :: IO ()
main = do
  putStrLn "hello world"


myMerge :: forall a xs ys comp.
           ([a] ~~ xs ::: SortedBy comp xs)
        -> ([a] ~~ ys ::: SortedBy (Op comp) ys)
        -> [a]
myMerge xs ys = mergeGDP xs (rev ys' `because` ok)
  where
    ys' :: [a] ~~ ys
    ys' = bare ys

    ok  :: Proof (SortedBy comp (Rev ys))
    ok = op_rev_lemma (contextOf ys)

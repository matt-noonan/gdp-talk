module Main where

import           Data.Ord
import           GDP.Merge (mergeGDP, sortGDP)
import           Named     (name)
import           The

myMergeDown :: [Int] -> [Int] -> [Int]
myMergeDown xs ys = name (comparing Down) $ \comp ->
    let xs' = sortGDP comp xs
        ys' = sortGDP comp ys

    in the (mergeGDP comp xs' ys')

main :: IO ()
main = do
  putStrLn "hello world"

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main where

import           Prelude            hiding (head, reverse, tail)

import           Data.Ord
import           GDP
import           GDP.Merge
import           Lists
import           Named
import           The

import           System.Environment (getArgs)


myMergeDown :: [Int] -> [Int] -> [Int]
myMergeDown xs ys = name (comparing Down) $ \comp ->
    let xs' = sortGDP comp xs
        ys' = sortGDP comp ys

    in the (mergeGDP comp xs' ys')

main :: IO ()
main = do
    args <- getArgs
    name args $ \args -> case args of
        Nil  -> putStrLn "No arguments!"
        Cons -> noting rev'cons args $ do
            print (head args)
            print (head (reverse args))

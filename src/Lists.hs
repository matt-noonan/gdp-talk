{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ViewPatterns        #-}

module Lists
    ( pattern Cons
    , pattern Nil
    , IsCons
    , IsNil
    , Reverse
    , head
    , tail
    , reverse
    , rev'cons) where

import           Prelude         hiding (head, reverse, tail)
import qualified Prelude         as Prelude

import           Data.Constraint
import           GDP
import           Named
import           The

data IsNil  xs
data IsCons xs

classify :: forall a xs. ([a] ~~ xs) -> Either (Dict (Fact (IsCons xs))) (Dict (Fact (IsNil xs)))
classify xs = case the xs of
    []    -> Right (note (axiom "IsNil"  :: Proof (IsNil  xs)) Dict)
    (_:_) -> Left  (note (axiom "IsCons" :: Proof (IsCons xs)) Dict)


pattern Nil  :: () => Fact (IsNil  xs) => ([a] ~~ xs)
pattern Nil <- (classify -> Right Dict)

pattern Cons :: () => Fact (IsCons xs) => ([a] ~~ xs)
pattern Cons <- (classify -> Left Dict)

newtype Reverse xs = Reverse Defn

head :: Fact (IsCons xs) => ([a] ~~ xs) -> a
head xs = Prelude.head (the xs)

tail :: Fact (IsCons xs) => ([a] ~~ xs) -> [a]
tail xs = Prelude.tail (the xs)

reverse :: ([a] ~~ xs) -> ([a] ~~ Reverse xs)
reverse xs = defn (Prelude.reverse (the xs))

rev'cons :: Proof (IsCons xs) -> Proof (IsCons (Reverse xs))
rev'cons _ = axiom "Reverse of a non-empty list is still non-empty."

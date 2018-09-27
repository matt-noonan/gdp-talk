{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module GDP
    ( type (:::)
    , type (?)
    , type (==)
    , type (&&)
    , type (||)
    , Not
    , Proof
    , True
    , False
    , refl
    , orIntroL
    , orIntroR
    , orElim
    , andIntro
    , andElimL
    , andElimR
    , absurd
    , axiom
    , contextOf
    , bare
    , because
    , noting
    , note
    , Fact
    ) where

import The
import Named
import Data.Coerce
import Unsafe.Coerce

data Proof p = QED

axiom :: String -> Proof p
axiom _ = QED

data True
data False

data Not p

data p && q
data p || q
data x == y

newtype SuchThat p x = SuchThat x
instance Functor (SuchThat p) where
    fmap = coerce
    
type x ::: p = SuchThat p x

because :: (x ::: p) -> (Proof p -> Proof q) -> (x ::: q)
x `because` proof = coerce x

contextOf :: (x ::: p) -> Proof p
contextOf _ = QED

bare :: (x ::: p) -> x
bare = coerce

newtype x ? p = Refined x

instance The x a => The (x ::: p) a where
    the = the . (coerce :: (x ::: p) -> x)

refl :: Proof (x == x)
refl = QED

orIntroL :: Proof p -> Proof (p || q)
orIntroL _ = QED

orIntroR :: Proof q -> Proof (p || q)
orIntroR _ = QED

orElim :: (Proof p -> Proof r)
       -> (Proof q -> Proof r)
       -> Proof (p || q)
       -> Proof r
orElim _ _ _ = QED

andIntro :: Proof p -> Proof q -> Proof (p && q)
andIntro _ _ = QED

andElimL :: Proof (p && q) -> Proof p
andElimL _ = QED

andElimR :: Proof (p && q) -> Proof q
andElimR _ = QED

absurd :: Proof False -> Proof p
absurd _ = QED

--------------------

{-
type Fact p = Given (Proof p)

using :: Fact p => (Proof p -> Proof q) -> (Fact q => t) -> t
using impl = give (impl given)

noting :: a => (a :- b) -> (b => r) -> r
noting impl k = k \\ impl

on :: (Proof (p n) -> Proof q) -> (a ~~ n) -> (Fact (p n) :- Fact q)
on impl _ = impl2sub impl

impl2sub :: forall p q. (Proof p -> Proof q) -> (Fact p :- Fact q)
impl2sub impl = Sub (give (impl (given :: Proof p)) Dict)

note :: Proof p -> (Fact p => t) -> t
note = give
-}

class Fact p

newtype WithFact p t = WithFact (Fact p => t)

trustme :: forall p t. (Fact p => t) -> t
trustme x = unsafeCoerce (WithFact x :: WithFact p t) ()

using :: forall p q t. Fact p => (Proof p -> Proof q) -> (Fact q => t) -> t
using _ = trustme @q

{-
noting :: a => (a :- b) -> (b => r) -> r
noting impl k = k \\ impl

on :: (Proof (p n) -> Proof q) -> (a ~~ n) -> (Fact (p n) :- Fact q)
on impl _ = impl2sub impl

impl2sub :: forall p q. (Proof p -> Proof q) -> (Fact p :- Fact q)
impl2sub impl = Sub ((trustme @p) Dict)
-}
note :: forall p t. Proof p -> (Fact p => t) -> t
note _ = trustme @p

noting :: forall p q a n r. (Proof (p n) -> Proof q) -> (a ~~ n) -> (Fact q => r) -> r
noting _ _ = trustme @q

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GDP
    ( type (:::)
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
    ) where

import The
import Data.Coerce

data Proof p = QED

axiom :: String -> Proof p
axiom _ = QED

data True
data False

data Not p

data p && q
data p || q
data x == y

newtype x ::: p = SuchThat x

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

test :: Proof (p && q) -> Proof (p || q)
test = orIntroL . andElimL

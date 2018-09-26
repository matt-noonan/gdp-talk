{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Named (name, type (~~), defn, Defn, Defining) where

import Data.Coerce
import The

newtype a ~~ name = Named a

instance The (a ~~ name) a

name :: a -> (forall name. (a ~~ name) -> t) -> t
name = flip coerce

-----------------------------------------------

data Defn

type Defining p = (Coercible p Defn, Coercible Defn p)

defn :: Defining p => a -> (a ~~ p)
defn = coerce

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Named (name, type (~~)) where

import Data.Coerce
import The

newtype a ~~ name = Named a

instance The (a ~~ name) a

name :: a -> (forall name. (a ~~ name) -> t) -> t
name = flip coerce

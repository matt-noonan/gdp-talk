module Named (name, type (~~)) where

import Data.Coerce

newtype a ~~ name = Named a

-- Forgetting names
instance The (a ~~ name) a where
    the = coerce :: (a ~~ name) -> a

-- Introducing names (encode existentials w/ a rank-2 type)
name :: a -> (forall name. (a ~~ name) -> t) -> t
--   :: a ->  exists name. (a ~~ name)
name x cont = cont (coerce x)

module Named (name, type (~~)) where
                      -- ^ Constructor NOT exported!
import Data.Coerce

newtype a ~~ name = Named a

-- Forgetting names
instance The (a ~~ name) a where
    the = coerce :: (a ~~ name) -> a

-- Introducing names
name :: a -> (forall name. (a ~~ name) -> t) -> t
--   :: a ->  exists name. (a ~~ name)
name x cont = cont (coerce x)

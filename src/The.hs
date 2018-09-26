{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module The (The(..)) where

import           Data.Coerce

class The a b | a -> b where
    the :: a -> b
    default the :: Coercible a b => a -> b
    the = coerce

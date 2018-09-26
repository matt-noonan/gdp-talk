newtype HashOf x = HashOf Defn

realHash :: Serializable a =>  a       ->  Hash
hash     :: Serializable a => (a ~~ x) -> (Hash ~~ HashOf x)

hash x = defn (realHash (serialize $ the x))

-- A type for objects along with their hash.
data ThingWithHash a = forall x. ThingWithHash
  { _thing :: a    ~~ x
  , _hash  :: Hash ~~ HashOf x }

-- Use it like this:
hashIt :: Serializable a => a -> ThingWithHash a
hashIt x = name x $ \x' ->
  ThingWithHash { _thing = x', _hash = hash x' }



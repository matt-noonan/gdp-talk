newtype Reverse xs = Reverse Defn

reverse :: ([a] ~~ xs) -> ([a] ~~ Reverse xs)
reverse xs = defn (Prelude.reverse (the xs))

rev'rev  :: Proof (xs == Reverse (Reverse xs))
rev'rev = axiom "reversing twice is identity"

rev'cons ::
  Proof (IsCons xs) -> Proof (IsCons (Reverse xs))
rev'cons _ = axiom "reverse preserves cons-ness"

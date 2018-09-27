-- module GDP, continued:

newtype SuchThat p x = SuchThat x deriving Functor

type x ::: p = SuchThat p x

because :: (x ::: p) -> (Proof p -> Proof q) -> (x ::: q)
x `because` proof = coerce x

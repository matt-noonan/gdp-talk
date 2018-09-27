module GDP (Proof, axiom, {- etc -}) where

data Proof p = QED

axiom :: String -> Proof p
axiom reason = QED

newtype a ::: p = WithContext a
instance The (a ::: p) a

toContext   :: Proof p -> a -> (a ::: p)
fromContext :: (a ::: p)    -> Proof p

class Fact p

note   :: Proof p -> (Fact p => t) -> t
recall :: (Fact p => t) -> Proof p

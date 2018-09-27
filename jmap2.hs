
data Keys m

data IsPresent k m where
    Present :: Fact (k #$\in$# Keys m) => KeyPresent k m
    Absent  :: KeyPresent k m

member :: Ord k =>
    (k ~~ key) -> (Map k v ~~ m) -> Maybe (IsPresent key m)
member k m = if Map.member (the k) (the m)
               then assert Present
               else        Absent

lookup :: (Ord k, Fact (key #$\in$# Keys m)) =>
       => (k ~~ key) -> (Map k v ~~ m) -> v
lookup key m = fromJust (Map.lookup (the k) (the m))

delete :: Ord k =>
  (k ~~ key) -> (Map k v ~~ m) -> (Map k v ~~ Delete key m)

delete'keys :: Proof (Keys (Delete key m) == Keys m \\ Singleton key)

subset'elt :: Proof (x #$\in$# a)
           -> Proof (a #$\subseteq$# b)
           -> Proof (x #$\in$# b)


-- Ghostly predicates
data Keys m
data x #$\in$# s

-- Key search, avoiding boolean blindness
member :: (k ~~ key)
       -> (Map k v ~~ m)
       -> Maybe (k ~~ key ::: key #$\in$# Keys m)
member k m = if Map.member k (the m)
               then Just (coerce k)
               else Nothing

-- Maybe-free lookup
lookup :: (k #$\in$# keys) -> JMap keys k v -> v
lookup k m = Map.lookup (the k) (the m)

-- A safe adjacency-list type for directed graphs
type Digraph k v = JMap keys k [k #$\in$# keys]

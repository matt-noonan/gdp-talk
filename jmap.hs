-- Ghosts of departed key sets
newtype JMap keys k v = JMap (Map k v)
newtype k #$\in$# keys      = Key k

-- Key search, avoiding boolean blindness
member :: Ord k => k -> JMap keys k v -> Maybe (k #$\in$# keys)
member k m = if Map.member k (the m)
               then Just (coerce k)
               else Nothing

-- Maybe-free lookup
lookup :: Ord k => (k #$\in$# keys) -> JMap keys k v -> v
lookup k m = fromJust (Map.lookup (the k) (the m))

-- A safe adjacency-list type for directed graphs
type Digraph k v = JMap keys k [k #$\in$# keys]

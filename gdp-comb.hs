-- module GDP, continued:
data p || q
data p && q
data x == y

orIntroL :: Proof p -> Proof (p || q)

orElim :: (Proof p        -> Proof r)
       -> (Proof q        -> Proof r)
       -> (Proof (p || q) -> Proof r)

andIntro :: Proof p -> Proof q -> Proof (p && q)

refl :: Proof (x == x)

-- ...and many, many more

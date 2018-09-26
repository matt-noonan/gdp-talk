data NonEmptyList a = NonEmptyList a [a]

headNE :: NonEmptyList a -> a
headNE (NonEmptyList x xs) = x

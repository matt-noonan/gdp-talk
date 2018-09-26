headMay :: [a] -> Maybe a
headMay = \case
    []     -> Nothing
    (x:xs) -> Just x

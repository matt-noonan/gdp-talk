data IsNil  xs
data IsCons xs

pattern Nil  :: () => Fact (IsNil  xs) => ([a] ~~ xs)
pattern Cons :: () => Fact (IsCons xs) => ([a] ~~ xs)

head :: Fact (IsCons xs) => ([a] ~~ xs) ->  a
head xs = Prelude.head (the xs)

tail :: Fact (IsCons xs) => ([a] ~~ xs) -> [a]
tail xs = Prelude.tail (the xs)

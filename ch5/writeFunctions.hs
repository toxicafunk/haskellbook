-- writeFunctions.hs

i :: a -> a
i = \a -> a

c :: a -> b -> a
c = \a b -> a

c'' :: b -> a -> b
c'' = \b a -> b

c' :: a ->b -> b
c' = \a b -> b

r :: [a] -> [a]
r = \lst -> reverse lst

co :: (b -> c) -> (a -> b) -> a -> c
co = \bToC aToB a -> bToC $ aToB a

a :: (a -> c) -> a -> a
a = \_ a -> a

a' :: (a -> b) -> a -> b
a' = \aToB a -> aToB a

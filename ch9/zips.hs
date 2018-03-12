-- zips.hs
module Zips where

zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys
zip' _ []          = []
zip' [] _          = []

zipWith' :: (a -> b -> c)
         -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
zipWith' _ [] _            = []
zipWith' _ _  []           = []

zip'' :: [a] -> [b] -> [(a,b)]
zip'' = zipWith' (,)

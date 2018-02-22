-- factorial.hs
module Factorial where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f (applyTimes (n-1) f b)

applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0 _ b =  b
applyTimes' n f b =  f . applyTimes' (n-1) f $ b

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes times = applyTimes' times (+ 1) 

--applyTimes 5 (+1) 5
--(+1) (applyTimes 4 (+1) 5)
--(+1) ((+1) (applyTimes 3 (+1) 5))
--(+1) ( (+1) ( (+1) applyTimes 2 (+1) 5) ) )
--(+1) ( (+1) ( (+1) ( (+1) applyTimes 1 (+1) 5) ) ) )
--(+1) ( (+1) ( (+1) ( (+1) ( (+1) applyTimes 0 (+1) 5) ) ) ) )
--(+1) ( (+1) ( (+1) ( (+1) ( (+1) (5) ) ) ) ) )

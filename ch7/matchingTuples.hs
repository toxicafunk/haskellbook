-- matchingTuples.hs
module TupleFunctions where

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f (a,b) (c,d) = ((b,d), (a,c))

addEmUp2 :: Num a => (a,a) -> a
addEmUp2 (x,y) = x + y

addEmUp2Alt :: Num a => (a,a) -> a
--addEmUp2Alt tup = fst tup + snd tup
addEmUp2Alt = uncurry (+) 

fst3 :: (a,b,c) -> a
fst3 (x, _, _) = x

third3 :: (a,b,c) -> c
third3 (_,_,x) = x

f' :: (a,b,c)
   -> (d,e,f)
   -> ((a,d),(c,f))
f' (a,b,c) (d,e,f) = ((a,d),(c,f))

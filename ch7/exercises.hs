-- exercises.hs
module Exercises where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d     = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' = r . f
  where
    f y = fst $ dTen y
    r z = snd $ dTen z
    dTen y = divMod y 10

data Choice = Tens | Hundreds

tensDigit'' :: Integral a => Choice -> a -> a
tensDigit'' c = case c of
  Tens     -> r . f
  Hundreds -> f . r
  where
    f y = fst $ dTen y
    r z = snd $ dTen z
    dTen y = divMod y 10

foldBool :: a -> a -> Bool -> a
foldBool x _ False = x
foldBool _ y True = y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b = case b of
  False -> x
  True -> y

foldBool'' :: a -> a -> Bool -> a
foldBool'' x y b
  | not b     = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

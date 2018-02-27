-- recursion.hs
module Recursion where

sumN :: (Eq a, Num a) => a -> a
sumN n = go 1 0
  where go x acc
          | x == n    = acc + x
          | otherwise = go (x + 1) (acc + x)

mult :: (Integral a) => a -> a -> a
mult n m = go 1 0
  where go x acc
          | x == n    = acc + m
          | otherwise = go (x + 1) (acc + m)

data DivideResult =
    Result Integer
  | DivideByZero
  deriving Show

dividedBy :: Integral a => a -> a -> DivideResult
dividedBy num denom
  | denom == 0            = DivideByZero
  | num >= 0 && denom > 0 = Result $ go num denom 0
  | num < 0 && denom < 0  = Result $ go (abs num) (abs denom) 0
  | otherwise             = Result $ negate $ go (abs num) (abs denom) 0
  where go n d count
          | n < d     = count
          | otherwise = go (n - d) d (count + 1)

mc91 :: Integer -> Integer
mc91 x
  | x > 100   = x -10
  | otherwise = 91


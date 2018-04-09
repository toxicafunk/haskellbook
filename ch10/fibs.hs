-- fibs.hs
module Fibs where

fibs    = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibs20 = take 20 $ 1: scanl (+) 1 fibs

fibsLess100 = takeWhile (< 100) fibs

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial' :: Integer -> [Integer]
factorial' n = 1 : scanl (\acc c -> acc * (c + 1)) 1 [1..n-1]


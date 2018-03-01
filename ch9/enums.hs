-- enums.hs
module Enums where

eftBool :: Bool -> Bool -> [Bool]
eftBool False True  = [False, True]
eftBool False False = [False]
eftBool True True   = [True]
eftBool _ _         = []

eftA :: (Ord a, Enum a) => a -> a -> [a]
eftA a b
  | a > b     = []
  | a == b    = [a]
  | otherwise = go a b []
  where
    go x y l
      | x == y    = x : l
      | otherwise = let z = pred y
                    in go x z (y : l)

eftBool' :: Bool -> Bool -> [Bool]
eftBool' = eftA

eftInt :: Int -> Int -> [Int]
eftInt = eftA

eftChar :: Char -> Char -> [Char]
eftChar = eftA

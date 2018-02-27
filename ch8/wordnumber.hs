-- wordnumber.hs
module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  _ -> "undefined"


digits :: Int -> [Int]
digits n = go n []
  where
    go x acc
      | x < 10    = x : acc
      | otherwise = go d (m : acc)
        where
          d = div x 10
          m = mod x 10

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord $ digits n

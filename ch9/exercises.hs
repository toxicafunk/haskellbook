-- exercises.hs
module Exercises where

import Data.Char

onlyUpper :: String -> String
onlyUpper = filter isUpper

capitalizeFirst :: String -> String
capitalizeFirst (x:xs) = toUpper x : xs
capitalizeFirst _      = ""

capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : capitalize xs

capitalizeFirst' :: String -> Char
capitalizeFirst' = toUpper . head

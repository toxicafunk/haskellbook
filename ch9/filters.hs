-- filters.hs
module Filters where

mult3 :: Integral a => [a] -> [a]
mult3 = filter (\x -> rem x 3 == 0)

sizeMult3 :: Integral a => [a] -> Int
sizeMult3 = length . mult3

removeArticles :: String -> [String]
removeArticles = filter (\x -> not (x == "the" || x == "an" || x == "a")) . words

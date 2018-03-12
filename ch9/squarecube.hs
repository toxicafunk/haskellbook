-- squarecube.hs
module SquareCube where

import Data.Bool

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..5]]

myCube :: [Integer]
myCube = [y^3 | y <- [1..5]]

myTuple :: [(Integer, Integer)]
myTuple = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]

foldBool :: (Num a, Eq a) => a -> [a] -> [a]
foldBool b = map (\x -> bool (-x) x (x == b))

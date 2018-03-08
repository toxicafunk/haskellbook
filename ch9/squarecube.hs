-- squarecube.hs
module SquareCube where

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..5]]

myCube :: [Integer]
myCube = [y^3 | y <- [1..5]]

myTuple :: [(Integer, Integer)]
myTuple = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]


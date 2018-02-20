{-# LANGUAGE NoMonomorphismRestriction #-}
-- determineTheType.hs
module DetermineTheType where

example = 1

t1 = (* 9) 6
t2 = head [(0,"doge"), (1,"kitteh")]
t3 = head [(0::Integer, "doge"),(1,"kitteh")]
t4 = if False then True else False
t5 = length [1,2,3,4,5]
t6 = (length [1,2,3,4]) > (length "TACOCAT")

x = 5
y = x + 5

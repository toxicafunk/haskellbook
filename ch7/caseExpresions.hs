-- caseExpressions.hs
module CaseExpressions where

funcZ x = case x + 1 == 1 of
  True -> "AWESOME"
  False -> "wut"

pal xs = case xs == reverse xs of
  True -> "yes"
  False -> "no"

pal' xs = case y of
  True -> "yes"
  False -> "no"
  where y = xs == reverse xs

functionC' x y = case x > y of
  True -> x
  False -> y

ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2' n = case even n of
  True -> (n+2)
  False -> n

nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

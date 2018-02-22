-- arith4.hs
module Arith4 where

-- id :: a -> a
-- id x = x

-- read :: Read a => String -> a
-- show :: Show a => a -> String

roundTrip :: (Show a, Read a) => a -> a
roundTrip = read . show

roundTrip' :: (Show a, Read b) => a -> b
roundTrip' = read . show

main = do
  print (roundTrip 22)
  print (id 22)
  print (roundTrip' 22 :: Int)

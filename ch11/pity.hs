-- pity.hs
module Pity where

import Prelude
import Data.Int

data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

myCardinality :: Enum a => [a] -> Int
myCardinality = foldr ((+) . length . enumFrom) 0

cardBigSmall :: Int
cardBigSmall =  myCardinality [False, False]

data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)

myNumba :: NumberOrBool
myNumba = Numba (-128)

cardMyNumba :: Int
cardMyNumba = myCardinality [(-128 :: Int8)] + cardBigSmall

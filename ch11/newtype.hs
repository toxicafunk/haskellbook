-- newtypes.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Newtypes where

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

--instance TooMany Goats where
  --tooMany (Goats n) = n > 43
  --tooMany (Goats n) = tooMany n

--type Cows = Cows Int

--instance TooMany Cows where
--  tooMany (Cows n) = n > 30

newtype StringTuple  = StringTuple (Int, String) deriving (Show, Eq)

instance TooMany StringTuple  where
  tooMany (StringTuple (i, _)) = tooMany i

newtype IntTuple  = IntTuple (Int, Int) deriving (Show, Eq)

instance TooMany IntTuple  where
  tooMany (IntTuple (i, i')) = tooMany $ i + i'

newtype MyTuple a = MyTuple (a, a) deriving (Show, Eq)

instance (Num a, TooMany a) => TooMany (MyTuple a) where
  tooMany (MyTuple (i, i')) = tooMany $ i * i'

-- Use FlexibleInstances

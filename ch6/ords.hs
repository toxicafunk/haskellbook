-- ords.hs
module Ords where

data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Eq, Show)

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

add :: DayOfWeek -> DayOfWeek -> Int
add d1 d2 = toInt d1 + toInt d2
  where
    toInt d = case d of
        Mon -> 1
        Tue -> 2
        Wed -> 3
        Thu -> 4
        Fri -> 5
        Sat -> 6
        Sun -> 7

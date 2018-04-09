module Database where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
             (secondsToDiffTime 34123)
           )
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123)
           )
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr filterDate []
  where
    filterDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
    filterDate (DbDate utcTime) acc = utcTime : acc
    filterDate _ acc                = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr filterNumber []
  where
    filterNumber :: DatabaseItem -> [Integer] -> [Integer]
    filterNumber (DbNumber num) acc = num : acc
    filterNumber _ acc          = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = foldr (\c acc -> if c < acc then acc else c) (head lst) (tail lst)
  where
    lst = filterDbDate db

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = fromIntegral (sumDb db) / fromIntegral (length (filterDbNumber db))

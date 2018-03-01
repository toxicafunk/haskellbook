-- portions.hs
module Portions where

myWords :: String -> [String]
myWords s = reverse $ go s []
  where
    go "" l = l
    go a l  = go (dropFirstBlank $ dropWhile isNotBlank a) (b : l)
      where
        b                 = takeWhile isNotBlank a
        isNotBlank        = (/=) ' '
        isBlank           = (==) ' '
        dropFirstBlank s1 = dropWhile isBlank $ dropWhile isNotBlank s1

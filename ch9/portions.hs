-- portions.hs
module Portions where

myWords :: Char -> String -> [String]
myWords sep s = reverse $ go s []
  where
    go "" l = l
    go a l  = go ((dropFirstBlank . dropNotBlank) a) (b : l)
      where
        b              = takeWhile isNotBlank a
        isNotBlank     = (/=) sep
        isBlank        = (==) sep
        dropNotBlank   = dropWhile isNotBlank
        dropFirstBlank = dropWhile isBlank . dropNotBlank

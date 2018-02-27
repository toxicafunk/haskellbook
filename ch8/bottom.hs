--bottom.hs
module Bottom where

f :: Bool -> Int
f True = error "blah"
f False = 0

f' :: Bool -> Maybe Int
f' False = Just 0
f' _     = Nothing

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

type Numerator   = Integer
type Denominator = Integer
type Quotient    = Integer
type Remainder   = Integer

dividedBy :: Numerator -> Denominator -> Quotient
dividedBy = div

dividedBy' :: Integral a => a -> a -> (a,a)
dividedBy' num denom = go num denom 0
  where go n d count
          | n < d     = (count, n)
          | otherwise = go (n - d) d (count + 1)

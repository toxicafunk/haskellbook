-- stdlibs.hs
module StdLibs where

myOr :: [Bool] -> Bool
myOr []     = False
myOr (True:_) = True
myOr (_:xs) = myOr xs

myOr' :: [Bool] -> Bool
myOr' []     = False
myOr' (x:xs) = x || myOr' xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ []     = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x' (x:xs) = x' == x  || myElem x' xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = any (== x)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = rev xs [x]
  where
    rev :: [a] -> [a] -> [a]
    rev []     acc' = acc'
    rev (y:ys) acc' = rev ys (y : acc')

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain []     = []
squishAgain (x:xs) = (squishMap (: []) x) ++ squishAgain xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = maxBy x xs
  where
    maxBy m []     = m
    maxBy m (y:ys) = case f m y of
      GT -> maxBy m ys
      _  -> maxBy y ys

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f (x:x':xs) = case f x x' of
  GT -> myMaximumBy' f (x:xs)
  _  -> myMaximumBy' f (x':xs)
myMaximumBy' _ (x:[])    = x
myMaximumBy' _ []        = undefined

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = maxBy x xs
  where
    maxBy m []     = m
    maxBy m (y:ys) = case f m y of
      LT -> maxBy m ys
      _  -> maxBy y ys

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare

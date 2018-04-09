-- exercises.hs
module Exercises where

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combs :: [(Char, Char, Char)]
combs = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

combs' :: [(Char, Char, Char)]
combs' = [(x,y,z) | x <- stops, y <- vowels, z <- stops, x == 'p']

nouns :: [String]
nouns = ["cat","dog","baby","car","ball", "keys"]

verbs :: [String]
verbs = ["run","sleep","kick","push","kiss"]

sentences :: [(String,String,String)]
sentences = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]

seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

seekritFunc' :: Fractional a => String -> a
seekritFunc' x = fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\c acc -> f c || acc) False

myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\c acc -> c == x || acc) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (==x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\c acc -> f c : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\c acc -> if f c then c : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\c acc -> f c ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f lst = foldl (\acc c -> if f acc c == GT then acc else c) h t
  where
    h = head lst
    t = tail lst

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f lst = foldl (\acc c -> if f acc c == LT then acc else c) h t
  where
    h = head lst
    t = tail lst


-- exercises.hs
module Exercises where

import Data.List (sort)

data Person = Person Bool

instance Show Person where
  show (Person isPerson) = show isPerson

printPerson :: Person -> IO()
printPerson = print

data Mood = Blah
          | Woot deriving Show

instance Eq Mood where
  (==) Blah Blah = True
  (==) Woot Woot = True
  (==) _ _       = False

settleDown :: Mood -> Mood
settleDown x = if x == Woot
                  then Blah
                  else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 :: Object -> Sentence
s1 = Sentence "dogs" "drool"

s2 :: Sentence
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Show, Eq)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

truth = Papu (Rocks "chomskydoz")
             (Yeah True)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p ==p'

i :: Num a => a
--i :: a
i = 1

--f :: Float
--f :: Num a => a
--f :: Fractional a => a
f :: RealFrac a => a
f = 1.0

--freud :: a -> a
--freud :: Ord a => a -> a
freud :: Int -> Int
freud x = x

myX = 1 :: Int

sigmund :: Int -> Int
--sigmund :: a -> a
--sigmund :: Num a => a -> a
sigmund x = myX

--jung :: Ord a => [a] -> a
--jung :: [Int] -> Int
--jung :: [Char] -> Char
jung :: Ord a => [a] -> a
jung xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)

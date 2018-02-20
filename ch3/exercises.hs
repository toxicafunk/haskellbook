-- exercises.hs
module Exercises where

append :: String -> String -> String
append s x = s ++ x

charAt :: String -> Int -> Char
charAt s i = s !! i

substring :: String -> Int -> String
substring s i = drop i s

thirdLetter :: String -> Char
thirdLetter s = charAt s 3

curryAwesome :: String
curryAwesome = "Curry is awesome"

letterIndex :: Int -> Char
letterIndex i = charAt curryAwesome i

rvrs :: String -> String
rvrs s = drop 9 s ++ (take 4 (drop 5 s)) ++ take 5 s

main :: IO()
main = print $ rvrs curryAwesome

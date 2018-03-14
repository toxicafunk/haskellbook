-- cipher.hs
module Cipher where

import Data.Char

encodeWord :: Int -> Bool -> String -> String
encodeWord amount toRight = map (shift amount toRight)
  where
    shift :: Int -> Bool -> Char -> Char
    shift 0 _ ch  = ch
    shift n False 'a' = shift (n-1) False 'z'
    shift n True 'z'  = shift (n-1) True 'a'
    shift n False 'A' = shift (n-1) False 'Z'
    shift n True 'Z'  = shift (n-1) True 'A'
    shift n dir ch  = shift (n-1) dir (if dir then succ ch else pred ch)

encode :: Int -> String -> String
encode i = unwords . map (encodeWord i True) . words

decode :: Int -> String -> String
decode i = unwords . map (encodeWord i False) . words

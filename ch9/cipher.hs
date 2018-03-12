-- cipher.hs
module Cipher where

import Data.Char

encodeWord :: String -> Int -> Bool -> String
encodeWord str amount toRight = map (shift amount toRight) str
  where
    shift :: Int -> Bool -> Char -> Char
    shift 0 _ ch  = ch
    shift n False 'a' = shift (n-1) False 'z'
    shift n True 'z'  = shift (n-1) True 'a'
    shift n dir ch  = shift (n-1) dir (if dir then succ ch else pred ch)

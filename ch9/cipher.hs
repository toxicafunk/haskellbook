-- cipher.hs
module Cipher where

import Data.Char

encodeWord :: Int -> Bool -> String -> String
encodeWord amount toRight = map (shift amount toRight)
  where
    shift :: Int -> Bool -> Char -> Char
    shift 0 _ ch      = ch
    shift n False 'a' = shift (n-1) False 'z'
    shift n True 'z'  = shift (n-1) True 'a'
    shift n False 'A' = shift (n-1) False 'Z'
    shift n True 'Z'  = shift (n-1) True 'A'
    shift n dir ch    = shift (n-1) dir (if dir then succ ch else pred ch)

encodeWord' :: Int -> Bool -> String -> String
encodeWord' amount toRight = map (shift amount toRight)
  where
    shift :: Int -> Bool -> Char -> Char
    shift n dir ch = if dir then shf (+n) else shf (flip (-) n)
      where
        shf f = (chr . (+d) . flip mod 26 . f . flip (-) d . ord)  ch
        d     = if isUpper ch then 65 else 97

encode :: Int -> Bool -> String -> String
encode i useSucc = unwords . map (f i True) . words
  where
    f = if useSucc then encodeWord else encodeWord'

decode :: Int -> Bool -> String -> String
decode i useSucc = unwords . map (f i False) . words
  where
    f = if useSucc then encodeWord else encodeWord'

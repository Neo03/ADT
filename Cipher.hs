module Cipher where

import Data.Char

vigenere :: String -> String -> String
vigenere xs ys = vigenere' xs (cycle ys)

vigenere' :: String -> String -> String
vigenere' [] _ = ""
vigenere' (' ':xs) cyp =  ' ' : vigenere xs cyp
vigenere' (x:xs) cyp@(y:ys) = docyp x y : vigenere' xs ys
  where
    base      = ord 'A'
    r         = 26
    dist c    = ord c - base
    docyp l m = chr $ (dist l + dist m) `mod` r + base

main :: IO ()
main = print $ vigenere "MEET AT DAWN" "ALLY" == "MPPR AE OYWY"

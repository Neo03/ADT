module Patterns where

import Data.Char (toUpper)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf [] ys = True
isSubsequenceOf _ []  = False
isSubsequenceOf xa@(x:xs) (y:ys)
    | x == y = isSubsequenceOf xs ys
    | otherwise = isSubsequenceOf xa ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords xs = map f $ words xs
  where
    f as@(s:st) = (as, toUpper s:st)

capitalizeWord :: String -> String
capitalizeWord = undefined

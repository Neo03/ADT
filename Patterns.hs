module Patterns where

import Data.Char (toUpper, isAlpha)
import Data.List (groupBy)
import Data.Function (on)

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
capitalizeWord [] = []
capitalizeWord (x:xs)
  | isAlpha x  = toUpper x : xs
  | otherwise  = x : capitalizeWord xs

capitalizeParagraph :: String -> String
capitalizeParagraph xs = concatMap capitalizeWord $ groupBy ((==)`on`(=='.')) xs

module Phone  where

import Data.Char
import Data.String.Utils
import Data.List
import Data.Foldable


data DaPhone = DaPhone [String]

convo :: [String]
convo = [
  "Wanna play 20 questions",
  "Ya",
  "You 1st haha",
  "Lol ok.Have u ever tasted alcohol, lol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok.Do you think I am pretty lol",
  "Lol ya",
  "Haha thanks just making sure rofl ur turn"
  ]

-- valid Buttons = "1234567890*#"
type Digit = Char

-- valid presses = 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone keys) c = if isUpper c then ('*', 1) : looks else looks
    where looks = look keys (toLower c)

look :: Eq a => [[a]] -> a ->[(Char, Int)]
look keys c = case asum $ zipWith (fmap . (,)) [0..] $ map (elemIndex c) keys of
  Nothing -> []
  Just (n, ind) -> [(head $ show n, ind + 1)]

cellPhoneDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhoneDead = foldMap . reverseTaps

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = sum.map snd

mostPopularLetter :: String -> Char
mostPopularLetter = head . longest . group . sort

longest :: [[a]] -> [a]
longest = maximumBy(\x y -> compare (length x) (length y))

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . filter isAlpha . concat

coolestWrd :: [String] -> String
coolestWrd = head . longest . group . sort . words . join " "

phone :: DaPhone 
phone = DaPhone keymap

keymap = [
          "0",
          "1",
          "abc2",
          "def3",
          "ghi4",
          "jkl5",
          "mno6",
          "pqrs7",
          "tuv8",
          "wxyz9"
          ]

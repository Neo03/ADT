module Phone  where

import Data.Char
--import Data.String.Utils
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
reverseTaps phone c  =
  if isUpper c
  then [('*', 1), look phone (toLower c)]
  else [look phone c]

look :: DaPhone -> Char -> (Digit, Presses)
look (DaPhone((digit, press):tl)) c =
  case elemIndex c of
    Just idx -> (digit, idx + 1)
    Nothing -> look (DaPhone tl) c

cellPhoneDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhoneDead = undefined

module Phone where

-- data ThePhone =
--     Two "ABC2"
--   | Three "DEF3"
--   | Four "GHI4"
--   | Five "JKL5"
--   | Six "MNO6"
--   | Seven "PQRS7"
--   | Eight "TUV8"
--   | Nine "WXYZ9"
--   | Star "*^"
--   | Zero "0+ "
--   | Hash "#.,"

-- 1)
data ThePhone = ThePhone

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u tasted alcohol",
   "Lol ya",
   "Wow ur cool hava. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

reverseTaps :: ThePhone -> Char -> [(Digit, Presses)]
reverseTaps = undefined

cellPhonesDead :: ThePhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = undefined

mostPopularLetter :: String -> Char
mostPopularLetter = undefined

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> Char
coolestWord = undefined

module MoodSwing where

data Mood = Blah | Woot deriving Show
-- data Mood = Blah | Woot 

-- instance (Show a) => Show (Mood) where
--  show Blah = "Blah"
--   show Woot = "Woot"

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _    = Blah

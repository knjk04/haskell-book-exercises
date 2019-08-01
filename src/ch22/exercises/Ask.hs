module Ask where

newtype Reader r a =
  Reader { runReader :: r -> a } 

-- implement the following function (no further description given)
ask :: Reader a a
ask = Reader id

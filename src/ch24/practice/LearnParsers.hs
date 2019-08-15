module LearnParsers where

import Text.Trifecta

-- unexpected is a way of throwing errors in trifecta
stop :: Parser a
stop = unexpected "stop"

  -- "read a single character '1'"
one = char '1'

-- read a character '1', then die
-- (>>) :: Monad m => m a -> m b -> m b
-- whatever char '1' returns gets discarded, but the effect it had remains
one' :: Parser Char
one' = one >> stop
-- equivalent to char '1' >> stop

-- read two characters, '1' and '2'
oneTwo = char '1' >> char '2'

-- read two characters, '1' and '2', then die
oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo':"
  testParse oneTwo'

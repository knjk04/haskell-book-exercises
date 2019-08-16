module ParsingPracticeExercise where

import Text.Trifecta

----- From LearnParsers.hs -----

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
oneTwo :: Parser Char
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

  pNL "q1One:"
  testParse q1One

  pNL "q1OneTwo:"
  testParse q1OneTwo

  pNL "q2:"
  testParseString $ p123 "1"
  testParseString $ p123 "12"
  testParseString $ p123 "123"

  pNL "q3:"
  testParseString $ q3 "1"
  testParseString $ q3 "12"
  testParseString $ q3 "123"

--------------------------------

-- 1) "There’s a combinator that’ll let us mark that we expect
-- an input stream to be finished at a particular point in
-- our parser. In the parsers library this is simply called eof
-- (end-of-file) and is in the Text.Parser.Combinators module.
-- See if you can make the one and oneTwo parsers fail because
-- they didn’t exhaust the input stream!"

-- make 'one' fail
q1One :: Parser Char
q1One = eof >> char '1'

-- make oneTwo fail
q1OneTwo :: Parser Char
q1OneTwo = eof >> oneTwo >> stop

-- 2) "Use string to make a Parser that parses "1", "12" and "123" out of the example input
-- respectively. Try combining it with stop too." One parser should be able to parse all three of
-- those things
-- Prelude> p123 "1"
-- Success 1
-- Prelude> p123 "12"
-- Success 12
-- Prelude> p123 "123"
-- Success 123

testParseString :: Parser String -> IO ()
testParseString p =
  print $ parseString p mempty "123"

p123 :: String -> Parser String
p123 p = string p

-- 3) make a Parser that does what String does using char
q3 :: String -> Parser String
q3 "" = stop
q3 (x:xs) = char x >> q3 xs

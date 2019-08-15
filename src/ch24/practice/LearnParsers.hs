module LearnParsers where

import Text.Trifecta

-- unexpected is a way of throwing errors in trifecta
stop :: Parser a
stop = unexpected "stop"

  -- "read a single character '1'"
one = char 1

-- read a character '1', then die
-- (>>) :: Monad m => m a -> m b -> m b
-- whatever char '1' returns gets discarded, but the effect it had remains
one' = one >> stop
-- equivalent to char '1' >> stop

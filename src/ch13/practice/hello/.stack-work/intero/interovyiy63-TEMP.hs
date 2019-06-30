module Return where

-- Invalid: return type is IO Bool, not Bool
-- twoo :: IO Bool
-- twoo = do c  <- getChar
--           c' <- getChar
--           c == c'

-- modified for readibilty
twoo :: IO Bool
twoo = do c  <- getChar
          putChar ' '
          c' <- getChar
          putStrLn ""
          return (c == c')

returnNothing :: IO ()
returnNothing = do c <- getChar
                   putChar ' '
                   c' <- getChar
                   putStrLn ""
                   if c == c'
                     then putStrLn "True"
                   else return ()

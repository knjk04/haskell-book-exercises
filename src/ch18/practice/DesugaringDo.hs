module DesugaringDo where

-- do is syntactic sugar for (>>=) (bind)
bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "Name, please: "
  name <- getLine
  putStrLn ("why, hello there: " ++ name)

-- (>>) is the sequencing operator: sequences two actions together and drops the
-- resulting value of the first action

-- 
bindingAndSequencing' :: IO ()
bindingAndSequencing' =
  putStrLn "Name, please: " >>
  getLine >>=
  \name ->
    putStrLn ("Why, hello there: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name, please: "
  name <- getLine

  putStrLn "age, please: "
  age <- getLine
  putStrLn ("why, hello there: " 
           ++ name ++ " who is "
           ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
  putStrLn "name, please: " >>
  getLine >>=

  \name ->
    putStrLn "age, please:" >>
    getLine >>=

    \age ->
      putStrLn ("why, hello there: "
               ++ name ++ " who is "
               ++ age ++ " years old.")

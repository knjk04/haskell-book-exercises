module HuttonsRazor where

data Expr
  = Lit Integer
  | Add Expr Expr

-- 1) reduce an expression to a final sum
eval :: Expr -> Integer
eval (Lit n)    = n
eval (Add e e') = (eval e) + (eval e')

printExpr :: Expr -> String
printExpr (Lit n)     = show n
printExpr (Add e1 e2) = (printExpr e1) ++ " + " ++ (printExpr e2)

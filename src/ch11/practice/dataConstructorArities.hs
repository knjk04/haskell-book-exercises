module DataConstructorsArities where

-- nullary
data Example0 =
  Example0
  deriving (Eq, Show)

-- unary
data Example1 =
  Example1 Int
  deriving (Eq, Show)

-- unary
data MyType = MyVal Int
  deriving (Eq, Show)

-- binary
data Example2 =
  Example2 Int String
  deriving (Eq, Show)

data Example = MakeExample deriving Show

data ExampleParameterised = MakeExampleP Int deriving Show

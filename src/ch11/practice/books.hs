module Books where

-- data Fiction = Fiction deriving Show

-- data Nonfiction = Nonfiction deriving Show

-- type constructors Fiction and Nonfiction have the same name, but they are not the same
-- the type constructors are arguments to FictionBook and NonfictionBook 
-- data BookType = FictionBook Fiction
--  | NonfictionBook Nonfiction
--  deriving Show

type AuthorName = String

-- data Author = Author (AuthorName, BookType)

-- rewritten in normal form (as a sum of products)
-- it's in normal form since now further evaluation can be performed until an operation
-- or computation is "done using these types"
data Author =
    Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)

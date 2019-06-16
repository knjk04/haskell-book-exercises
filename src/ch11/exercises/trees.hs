module Trees where

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) 
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)
  
-- 1) write map for BinaryTree
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf)
       2
       (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "OK!"
  else error "test failed!"

-- convert binary trees to lists

preorder :: BinaryTree a -> [a]
preorder Leaf                = []
preorder (Node left a right) = a : (preorder left) ++ (preorder right)

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

inorder :: BinaryTree a -> [a]
inorder Leaf                = []
inorder (Node left a right) = (inorder left) ++ [a] ++ (inorder right)

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

postorder :: BinaryTree a -> [a]
postorder Leaf                = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- write foldr for BinaryTree
-- answer from https://github.com/bitemyapp/haskell-book-exercises/blob/gh-pages/ch11/ch11_11.15_2_0.hs
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f acc tree =
  case tree of
    Leaf -> acc
    (Node left a right) -> foldr f acc (flattenIn tree [])
    -- (Node left a right) -> (take 1 $ (postorder left) ++ [a] ++ (postorder right))

flattenIn :: BinaryTree a -> [a] -> [a]
flattenIn Leaf xs                = xs
flattenIn (Node left a right) xs = flattenIn left (a : (flattenIn right xs))


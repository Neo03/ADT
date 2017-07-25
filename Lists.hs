--module Tree_to_Lists where
module Main where
data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Ord, Eq, Show)

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left  a right ) = a : (preorder left  ++ preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: Ord a => BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO()
testPreorder = if preorder testTree == [2,1,3]
  then print "test Preorder passed!"
  else print "test Preorder failed."

testInOrder :: IO()
testInOrder = if inorder testTree == [1,2,3]
  then print "test InOrder passed!"
  else print "test InOrder failed."

testPostOrder :: IO()
testPostOrder = if postorder testTree == [1,3,2]
  then print "test postOrder passed!"
  else print "test postOrder failed."

--module Main where

main :: IO ()
main = do
    testPreorder
    testInOrder
    testPostOrder

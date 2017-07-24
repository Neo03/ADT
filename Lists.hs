--module Tree_to_Lists where
module Main where
data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Ord, Eq, Show)

preorder :: BinaryTree a -> [a]
preorder = undefined

inorder :: BinaryTree a -> [a]
inorder = undefined

postorder :: BinaryTree a -> [a]
postorder = undefined

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO()
testPreorder = if preorder testTree == [2,1,3]
  then print "testPreorder passed!"
  else print "testPreorder failed."

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

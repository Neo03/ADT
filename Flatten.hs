module Main where

main :: IO ()
main = do
  testPreorder
  testInOrder
  testPostOrder

data BinaryTree a =
   Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Ord, Eq, Show)

preorder :: BinaryTree a -> [a]
preorder bt = flattenPre bt []

flattenPre :: BinaryTree a -> [a] -> [a]
flattenPre Leaf l = l
flattenPre (Node left a right) l = a : flattenPre left (flattenPre right l)

inorder :: BinaryTree a -> [a]
inorder bt = flattenIn bt []

flattenIn :: BinaryTree a -> [a] -> [a]
flattenIn Leaf l = l
flattenIn (Node left a right) l = flattenIn left (a : flattenIn right l)

postorder :: BinaryTree a -> [a]
postorder bt = flattenPost bt []

flattenPost :: BinaryTree a -> [a] -> [a]
flattenPost Leaf l = l
flattenPost (Node left a right) l = flattenPost left (flattenPost right (a:l))

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO()
testPreorder = if preorder testTree == [2,1,3]
  then print "testPreorder passed!"
  else print "testPreorder failed!"

testInOrder :: IO()
testInOrder = if inorder testTree == [1,2,3]
  then print "testInOrder passed!"
  else print "testInOrder failed!"

testPostOrder :: IO()
testPostOrder = if postorder testTree == [1,3,2]
  then print "testPostOrder passed!"
  else print "testPostOrder failed!"

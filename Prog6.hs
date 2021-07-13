{- ################
   Hannah Metzler
    Prog6.hs
    11/14/19
  ###############-}

module Prog6 where

data Tree1 = Leaf1 Int
            | Node1 Int Tree1 Tree1

--preorder takes a tree arg and returns as list an inorder traversal of tree
preorder :: Tree1 -> [Int]
preorder (Leaf1 x) = [x]
preorder (Node1 n left right) = n : preorder left ++ preorder right

--postoder takes a tree arg and returns as list an inorder traversal of tree
postorder :: Tree1 -> [Int]
postorder (Leaf1 x) = [x]
postorder (Node1 n left  right) = postorder left ++ postorder right ++ [n]

--sumPositives takes a tree arg and returns the sum of positive ints in tree
sumPositives :: Tree1 -> Int
sumPositives (Leaf1 x)
  | x > 0 = x
  | otherwise = 0
sumPositives (Node1 n left right)
  | n > 0 = n + (sumPositives left) + (sumPositives right)
  | otherwise = (sumPositives left) + (sumPositives right)

--countLeaves returns the number of leaves in the tree
countLeaves :: Tree1  -> Int
countLeaves (Leaf1 a) = 1
countLeaves (Node1 n left right) = countLeaves left + countLeaves right

--depth returns the depth of a tree (tree w only a root has depth of 1)
depth :: Tree1 -> Int
depth (Leaf1 _) = 1
depth (Node1 n left right) = 1 + max (depth left) (depth right)

data Tree2 a = Leaf2 a
             | Node2 [Tree2 a]

--occurs returns whether a given arg is present in a given tree
occurs :: Eq a => a -> Tree2 a -> Bool
occurs x (Leaf2 l) = True
occurs x (Node2 (y:ys)) = occurs x y || occurs x (Node2 ys) 

--countInteriorNodes returns the number of interior nodes in tree
countInteriorNodes :: Tree2 a -> Int
countInteriorNodes (Leaf2 a) = 0
countInteriorNodes (Node2 (x:xs)) = 1 + countInteriorNodes x + countInteriorNodes (Node2 xs)

--sumTree takes a tree of ints and returns sum of all ints in tree
sumTree :: Tree2 Int -> Int
sumTree (Leaf2 a) = a
sumTree (Node2 (x:xs)) = sumTree x + sumTree (Node2 xs) 

--pre2 returns a preorder traversal of the nodes in tree
pre2 :: Tree2 a -> [a]
pre2 (Leaf2 n) = [n]
pre2 (Node2 [])= []
pre2 (Node2 (x:xs)) = pre2(Node2 xs) ++ pre2 x
--depthK returns all nodes that are at depth k in tree
depthK :: Int -> Tree2 a -> [a]
depthK 1 (Leaf2 a) = [a]
depthK k (Leaf2 a) = []
depthK k (Node2 []) = []
depthK k (Node2 (y:ys)) = depthK (k-1) y ++ depthK k (Node2 ys)


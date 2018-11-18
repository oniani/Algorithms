{- |
Module      :  BinaryTreeNode.hs
Description :  Module implements the binary tree node data structure
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Binary_tree#Nodes_and_references
-}

module BinaryTreeNode
    ( TreeNode (..)
    , nullNode
    , newNode
    , getValue
    , setValue
    , getLeft
    , getRight
    , setLeft
    , setRight
    ) where

data TreeNode = NULL | TreeNode (Integer, TreeNode, TreeNode) deriving (Eq, Show)


-- | Create the NULL node : O(1) complexity
nullNode :: TreeNode
nullNode = NULL

-- | Create the new node : O(1) complexity
newNode :: Integer -> TreeNode
newNode value = TreeNode (value, NULL, NULL)

-- | Get the value of the given node : O(1) complexity
getValue :: TreeNode -> Integer
getValue NULL = error "Cannot get the value of the NULL node!"
getValue (TreeNode (value, leftChild, rightChild)) = value

-- | Set the value of the given node : O(1) complexity
setValue :: TreeNode -> Integer -> TreeNode
setValue NULL value = TreeNode (value, NULL, NULL)
setValue (TreeNode (value, leftChild, rightChild)) newValue = TreeNode (newValue, leftChild, rightChild)

-- | Get the left child of the node : O(1) complexity
getLeft :: TreeNode -> TreeNode
getLeft (TreeNode (value, leftChild, rightChild)) = leftChild

-- | Get the right child of the node : O(1) complexity
getRight :: TreeNode -> TreeNode
getRight (TreeNode (value, leftChild, rightChild)) = rightChild

-- | Change the value of the left child of the node : O(1) complexity
setLeft :: TreeNode -> Integer -> TreeNode
setLeft (TreeNode (value, leftChild, rightChild)) newValue = TreeNode (value, setValue leftChild newValue, rightChild)

-- | Change the value of the right child of the node : O(1) complexity
setRight :: TreeNode -> Integer -> TreeNode
setRight (TreeNode (value, leftChild, rightChild)) newValue = TreeNode (value, leftChild, setValue rightChild newValue)


main = do
    print nullNode
    let node1 = newNode 1
    print node1
    print (getValue node1)
    let node2 = setValue node1 3
    print node2
    print (getValue node2)
    print (getLeft node1)
    print (getRight node1)
    let node3 = setLeft node1 10
    print node3
    let node4 = setRight node3 15
    print node4
    print (node1 == node2)
    print (node1 == node1)

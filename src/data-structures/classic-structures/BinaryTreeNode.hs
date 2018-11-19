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
    , getChildrenNumber
    , isNullNode
    ) where

data TreeNode = NULL | TreeNode Integer TreeNode TreeNode deriving (Eq, Show)


-- | Create the NULL node : O(1) complexity
nullNode :: TreeNode
nullNode = NULL

-- | Create the new node : O(1) complexity
newNode :: Integer -> TreeNode
newNode nodeValue = TreeNode nodeValue NULL NULL

-- | Get the value of the given node : O(1) complexity
getValue :: TreeNode -> Integer
getValue NULL = error "Cannot get the value of the NULL node!"
getValue (TreeNode nodeValue leftChild rightChild) = nodeValue

-- | Set the value of the given node : O(1) complexity
setValue :: TreeNode -> Integer -> TreeNode
setValue NULL nodeValue = TreeNode nodeValue NULL NULL
setValue (TreeNode nodeValue leftChild rightChild) newValue = TreeNode newValue leftChild rightChild

-- | Get the left child of the node : O(1) complexity
getLeft :: TreeNode -> TreeNode
getLeft (TreeNode nodeValue leftChild rightChild) = leftChild

-- | Get the right child of the node : O(1) complexity
getRight :: TreeNode -> TreeNode
getRight (TreeNode nodeValue leftChild rightChild) = rightChild

-- | Change the value of the left child of the node : O(1) complexity
setLeft :: TreeNode -> Integer -> TreeNode
setLeft (TreeNode nodeValue leftChild rightChild) newValue = TreeNode nodeValue (setValue leftChild newValue) rightChild

-- | Change the value of the right child of the node : O(1) complexity
setRight :: TreeNode -> Integer -> TreeNode
setRight (TreeNode nodeValue leftChild rightChild) newValue = TreeNode nodeValue leftChild (setValue rightChild newValue)

-- | Get the number of the children of the given node : O(1) comlexity
getChildrenNumber :: TreeNode -> Integer
getChildrenNumber (TreeNode nodeValue leftChild rightChild)
    | leftChild == NULL && rightChild == NULL = 0
    | leftChild == NULL || rightChild == NULL = 1
    | otherwise = 2

-- | Check if the given node is a null node
isNullNode :: TreeNode -> Bool
isNullNode NULL = True
isNullNode _ = False


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
    putStr "\n"
    print (node1 == node2)
    print (node1 == node1)
    putStr "\n"
    print node1
    print node2
    print node3
    print node4
    putStr "\n"
    print (getChildrenNumber node1)
    print (getChildrenNumber node2)
    print (getChildrenNumber node3)
    print (getChildrenNumber node4)
    putStr "\n"
    print (isNullNode nullNode)
    print (isNullNode node1)
    print (isNullNode node3)

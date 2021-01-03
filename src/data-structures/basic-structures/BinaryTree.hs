{- |
Module      :  BinaryTree.hs
Description :  Module implements the binary tree data structure
Copyright   :  (c) David Oniani
License     :  MIT License

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Binary_tree
-}

module BinaryTree
    ( BinaryTree (..)
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

data BinaryTree = NULL | BinaryTree Integer BinaryTree BinaryTree deriving (Eq, Show)


-- | Create the NULL node
nullNode :: BinaryTree
nullNode = NULL

-- | Create the new node
newNode :: Integer -> BinaryTree
newNode nodeValue = BinaryTree nodeValue NULL NULL

-- | Get the value of the given node
getValue :: BinaryTree -> Integer
getValue NULL = error "Cannot get the value of the NULL node!"
getValue (BinaryTree nodeValue leftChild rightChild) = nodeValue

-- | Set the value of the given node
setValue :: BinaryTree -> Integer -> BinaryTree
setValue NULL nodeValue = BinaryTree nodeValue NULL NULL
setValue (BinaryTree nodeValue leftChild rightChild) newValue = BinaryTree newValue leftChild rightChild

-- | Get the left child of the node
getLeft :: BinaryTree -> BinaryTree
getLeft (BinaryTree nodeValue leftChild rightChild) = leftChild

-- | Get the right child of the node
getRight :: BinaryTree -> BinaryTree
getRight (BinaryTree nodeValue leftChild rightChild) = rightChild

-- | Change the value of the left child of the node
setLeft :: BinaryTree -> Integer -> BinaryTree
setLeft (BinaryTree nodeValue leftChild rightChild) newValue = BinaryTree nodeValue (setValue leftChild newValue) rightChild

-- | Change the value of the right child of the node
setRight :: BinaryTree -> Integer -> BinaryTree
setRight (BinaryTree nodeValue leftChild rightChild) newValue = BinaryTree nodeValue leftChild (setValue rightChild newValue)

-- | Get the number of the children of the given node
getChildrenNumber :: BinaryTree -> Integer
getChildrenNumber (BinaryTree nodeValue leftChild rightChild)
    | leftChild == NULL && rightChild == NULL = 0
    | leftChild == NULL || rightChild == NULL = 1
    | otherwise = 2

-- | Check if the given node is a null node
isNullNode :: BinaryTree -> Bool
isNullNode NULL = True
isNullNode _ = False


main :: IO ()
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

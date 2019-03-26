{- |
Module      :  BinaryTreeNode.hs
Description :  Module implements the linked list data structure
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Linked_list
-}

module LinkedList
    ( LinkedList (..)
    , nullNode
    , listLength
    , appendNode
    , insertNode
    , deleteNode
    , deleteNodeAt
    ) where


data LinkedList = NULL | LinkedList Integer LinkedList deriving (Eq, Show)


-- | Initialize the head of the linked list
nullNode :: LinkedList
nullNode = NULL

-- | Get the length of the linked list
listLength :: LinkedList -> Integer
listLength NULL = 0
listLength (LinkedList nodeValue nextNode) = listLength nextNode + 1

-- | Append the node with the given value
appendNode :: LinkedList -> Integer -> LinkedList
appendNode NULL insertValue = LinkedList insertValue NULL
appendNode (LinkedList nodeValue nextNode) insertValue
    | nextNode == NULL = LinkedList nodeValue (LinkedList insertValue NULL)
    | otherwise = LinkedList nodeValue (appendNode nextNode insertValue)

-- | Insert the node at the given position
insertNode :: LinkedList -> Integer -> Integer -> LinkedList
insertNode NULL insertValue _ = LinkedList insertValue NULL
insertNode (LinkedList nodeValue nextNode) insertValue insertPosition
    | insertPosition == 0 = LinkedList insertValue (LinkedList nodeValue nextNode)
    | otherwise = LinkedList nodeValue (insertNode nextNode insertValue (insertPosition - 1))

-- | Delete the first node with the given value: proceeds from the left to the right
deleteNode :: LinkedList -> Integer -> LinkedList
deleteNode NULL _ = NULL
deleteNode (LinkedList nodeValue nextNode) deleteValue
    | nodeValue == deleteValue = nextNode
    | otherwise = LinkedList nodeValue (deleteNode nextNode deleteValue)

-- | Delete the node at the given index
deleteNodeAt :: LinkedList -> Integer -> LinkedList
deleteNodeAt NULL _ = NULL
deleteNodeAt (LinkedList nodeValue nextNode) deleteIndex
    | deleteIndex == 0 = nextNode
    | otherwise = LinkedList nodeValue (deleteNodeAt nextNode (deleteIndex - 1))


main :: IO ()
main = do
    let linkedList = nullNode
    let linkedList1 = appendNode linkedList 1
    let linkedList2 = appendNode linkedList1 2
    let linkedList3 = appendNode linkedList2 3
    let linkedList4 = appendNode linkedList3 4
    let linkedList5 = appendNode linkedList4 5
    putStrLn "Empty List"
    print linkedList
    putStr "\n"
    putStrLn "Inserting values..."
    print linkedList1
    print linkedList2
    print linkedList3
    print linkedList4
    print linkedList5
    putStr "\n"
    putStrLn "Inserting nodes index 0..."
    print (insertNode linkedList1 12 0)
    print (insertNode linkedList2 12 0)
    print (insertNode linkedList3 12 0)
    print (insertNode linkedList4 12 0)
    print (insertNode linkedList5 12 0)
    putStr "\n"
    putStrLn "Inserting nodes index 1..."
    print (insertNode linkedList1 12 1)
    print (insertNode linkedList2 12 1)
    print (insertNode linkedList3 12 1)
    print (insertNode linkedList4 12 1)
    print (insertNode linkedList5 12 1)
    putStr "\n"
    putStrLn "Finding out the lengths of the lists..."
    print (listLength linkedList1)
    print (listLength linkedList2)
    print (listLength linkedList3)
    print (listLength linkedList4)
    print (listLength linkedList5)
    putStr "\n"
    putStrLn "Deleting nodes..."
    print (deleteNode linkedList5 1)
    print (deleteNode linkedList5 2)
    print (deleteNode linkedList5 3)
    print (deleteNode linkedList5 4)
    print (deleteNode (deleteNode linkedList5 5) 2)
    putStr "\n"
    putStrLn "Deleting nodes at the given position..."
    print (deleteNodeAt linkedList5 0)
    print (deleteNodeAt linkedList5 1)
    print (deleteNodeAt linkedList5 2)
    print (deleteNodeAt linkedList5 3)
    print (deleteNodeAt linkedList5 4)

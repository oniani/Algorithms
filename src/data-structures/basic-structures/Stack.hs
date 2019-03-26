{- |
Module      :  Stack.hs
Description :  Module implements the stack data structure
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Stack_(abstract_data_type)
-}

module Stack
    ( Stack (..)
    , empty
    , push
    , top
    , pop
    , clear
    , Stack.null
    ) where

import qualified Data.Sequence as S

newtype Stack = Stack (S.Seq Integer) deriving (Eq)

-- Show the stack
instance Show Stack
    where
        show (Stack x) = "Stack " ++ drop 9 (show x)


-- | Create the empty stack
empty :: Stack
empty = Stack S.empty

-- | Get the size of the stack
size :: Stack -> Integer
size (Stack x) = toInteger (S.length x)

-- | Push the item onto the stack
push :: Stack -> Integer -> Stack
push (Stack x) y = Stack (x S.|> y)

-- | Get the top of the stack
top :: Stack -> Integer
top (Stack x)
    | S.null x = error "Cannot get the top of the empty stack!"
    | otherwise = S.index x (length x - 1)

-- | Pop the item from the the stack
pop :: Stack -> Stack
pop (Stack x)
    | S.null x = error "Cannot pop the empty stack!"
    | otherwise = Stack (S.deleteAt (length x - 1) x)

-- | Clear the stack
clear :: Stack -> Stack
clear (Stack x) = Stack S.empty

-- | Check if the stack is empty
null :: Stack -> Bool
null (Stack x) = S.null x


main :: IO ()
main = do
    let stack1 = Stack (S.fromList [0..9])
    let stack2 = pop stack1
    let stack3 = push stack1 2
    let stack4 = clear stack1
    print empty
    print stack1
    print stack2
    print stack3
    print stack4
    putStr "\n"
    print (size stack1)
    print (size stack2)
    print (size stack3)
    print (size stack4)
    putStr "\n"
    print (top stack1)
    print (top stack2)
    print (top stack3)
    -- print (top stack4)  -- Error: Cannot get the top of the empty stack!
    putStr "\n"
    print (Stack.null stack1)
    print (Stack.null stack2)
    print (Stack.null stack3)
    print (Stack.null stack4)
    print(stack1 == stack1)
    print(stack1 == stack2)

{- |
Module      :  Queue.hs
Description :  Module implements the queue data structure
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Queue_(abstract_data_type)
-}

module Queue
    ( Queue (..)
    , empty
    , enqueue
    , bottom
    , dequeue
    , clear
    , Queue.null
    ) where

import qualified Data.Sequence as S

newtype Queue = Queue (S.Seq Integer) deriving (Eq)

-- Show the queue
instance Show Queue
    where
        show (Queue x) = "Queue " ++ drop 9 (show x)


-- | Create the empty queue : O(1) complexity
empty :: Queue
empty = Queue S.empty

-- | Get the size of the queue : O(1) complexity
size :: Queue -> Int
size (Queue x) = S.length x

-- | Push the item onto the queue : O(1) complexity
enqueue :: Queue -> Integer -> Queue
enqueue (Queue x) y = Queue (y S.<| x)

-- | Get the bottom of the queue : O(1) complexity
bottom :: Queue -> Integer
bottom (Queue x)
    | S.null x = error "Cannot get the bottom of the empty queue!"
    | otherwise = S.index x 0

-- | Pop the item from the the queue : O(1) complexity
dequeue :: Queue -> Queue
dequeue (Queue x)
    | S.null x = error "Cannot dequeue the empty queue!"
    | otherwise = Queue (S.deleteAt 0 x)

-- | Clear the queue : O(1) complexity
clear :: Queue -> Queue
clear (Queue x) = Queue S.empty

-- | Check if the queue is empty : O(1) complexity
null :: Queue -> Bool
null (Queue x) = S.null x


main = do
    let queue1 = Queue (S.fromList [0..9])
    let queue2 = dequeue queue1
    let queue3 = enqueue queue1 2
    let queue4 = clear queue1
    print empty
    print queue1
    print queue2
    print queue3
    print queue4
    putStr "\n"
    print (size queue1)
    print (size queue2)
    print (size queue3)
    print (size queue4)
    putStr "\n"
    print (bottom queue1)
    print (bottom queue2)
    print (bottom queue3)
    -- print (bottom queue4)  -- Error: Cannot get the top of the empty queue!
    putStr "\n"
    print (Queue.null queue1)
    print (Queue.null queue2)
    print (Queue.null queue3)
    print (Queue.null queue4)
    print(queue1 == queue1)
    print(queue1 == queue2)

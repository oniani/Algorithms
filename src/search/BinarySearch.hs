{- |
Module      :  BinarySearch.hs
Description :  Module implements the binary search algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Binary_search_algorithm
-}

module BinarySearch where


-- | Binary search
binarySearch :: (Ord a) => [a] -> Int -> Int -> a -> Int
binarySearch x l r i
    | null x = -1                                  -- Checking list emptiness
    | l < 0 || r > length x - 1 = -1               -- Checking index bounds
    | i < head x || i > last x = -1                -- Checking value bounds
    | x !! mid == i = mid                          -- Base case
    | x !! mid > i = binarySearch x l (mid - 1) i  -- Left recursive case
    | x !! mid < i = binarySearch x (mid + 1) r i  -- Right recursive case
    where
        mid = quot (l + r) 2


main = do
    let arr = [1..100]
    putStr "The element with the value of 10 is at the index "
    print (binarySearch arr 0 99 10)  -- Prints out 9

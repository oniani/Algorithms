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
binarySearch :: (Ord a) => [a] -> a -> Int -> Int -> Int
binarySearch x i l r
    | l > r = -1                                   -- Check if left is greater than right
    | x !! mid == i = mid                          -- Base case
    | x !! mid > i = binarySearch x i l (mid - 1)  -- Left recursive case
    | x !! mid < i = binarySearch x i (mid + 1) r  -- Right recursive case
    | otherwise = -1
    where
        mid = quot (l + r) 2


main = do
    let arr = [1..100]
    putStr "The element with the value of 10 is at the index "
    print (binarySearch arr 10 0 99)  -- Prints out 9

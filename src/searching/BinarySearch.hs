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

import qualified Data.Vector as V


-- | Binary search
binarySearch :: (Ord a) => V.Vector a -> a -> Int -> Int -> Int
binarySearch x i l r
    | l > r = -1
    | guess == i = mid
    | guess > i = binarySearch x i l (mid - 1)
    | guess < i = binarySearch x i (mid + 1) r
    | otherwise = -1
    where
        mid = quot (l + r) 2
        guess = x V.! mid


main = do
    let arr = V.fromList [1..100]
    putStr "The element with the value of 10 is at the index "
    print (binarySearch arr 10 0 99)

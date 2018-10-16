{- |
Module      :  ExponentialSearch.hs
Description :  Module implements the exponential search algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Exponential_search
-}

module ExponentialSearch where

import BinarySearch (binarySearch)


exponentialSearch :: (Ord a) => [a] -> a -> Int -> Int
exponentialSearch x i s
    | s == 0 = -1
    | b >= s || x !! b >= i = multiplyByTwo b
    | otherwise = binarySearch x i (b `div` 2) (min (b + 1) s)
    where
        b = 1
        multiplyByTwo b = 2 * b


main = do
    let arr = [1..100]
    putStr "The element with the value of 10 is at the index "
    print (binarySearch arr 10 0 99)  -- Prints out 9

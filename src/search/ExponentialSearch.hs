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

import qualified Data.Vector as V
import BinarySearch (binarySearch)


exponentialSearch :: (Ord a) => V.Vector a -> a -> Int -> Int -> Int
exponentialSearch x i l r
    | x V.! l == i = l
    | (l + 1) <= r && x V.! (l + 1) < i = exponentialSearch x i (2 * (l + 1)) r
    | otherwise = binarySearch x i ((l + 1) `div` 2) (min (l + 1) r)


main = do
    let arr = V.fromList [1..100]
    putStr "The element with the value of 10 is at the index "
    print (exponentialSearch arr 10 0 99)  -- Prints out 9

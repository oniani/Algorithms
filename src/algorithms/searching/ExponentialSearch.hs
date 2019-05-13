{- |
Module      :  ExponentialSearch.hs
Description :  Module implements the exponential search algorithm
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Exponential_search
-}

module ExponentialSearch where

import Data.Vector (Vector, (!), fromList)
import BinarySearch (binarySearch)


exponentialSearch :: (Ord a) => Vector a -> a -> Integer -> Integer -> Integer
exponentialSearch x i l r
    | x ! fromInteger l == i = l
    | (l + 1) <= r && x ! fromInteger (l + 1) < i = exponentialSearch x i (2 * (l + 1)) r
    | otherwise = binarySearch x i ((l + 1) `div` 2) (min (l + 1) r)


main :: IO ()
main = do
    let arr = fromList [1..100]
    putStr "The element with the value of 10 is at the index "
    print (exponentialSearch arr 10 0 99)

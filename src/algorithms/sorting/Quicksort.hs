{- |
Module      :  Quicksort.hs
Description :  Module implements the quicksort algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Quicksort
-}

module Quicksort where


-- | Quicksort
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort (filter (<= x) xs) ++ [x] ++ quicksort (filter (> x) xs)


main = do
    let arr = [12,1,6,31,99,25,3,56,21,6]
    putStr "The sorted version of the array is "
    print (quicksort arr)

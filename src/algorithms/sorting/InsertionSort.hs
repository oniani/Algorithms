{- |
Module      :  InsertionSort.hs
Description :  Module implements the insertion sort algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Insertion_sort
-}

module InsertionSort where


-- | Insertion sort
insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort x = foldr insert [] x
    where
        insert y [] = [y]
        insert y (z:zs)
            | y <= z = y : (z:zs)
            | y > z = z : insert y zs


main = do
    let arr = [12,1,6,31,99,25,3,56,21,6]
    putStr "The sorted version of the array is "
    print (insertionSort arr)

{- |
Module      :  Quicksort.hs
Description :  Module implements the quick sort algorithm
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
quicksort x
    | length x < 2 = x                                            -- Base case
    | otherwise = quicksort less ++ [pivot] ++ quicksort greater  -- Recursive case
    where
        pivot = head x
        less = [i | i <- tail x, i <= pivot]
        greater = [i | i <- tail x, i > pivot]


main = do
    let arr = [12, 1, 6, 31, 99, 25, 3, 56, 21, 6]
    putStr "The sorted version of the array is "
    print (quicksort arr)

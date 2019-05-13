{- |
Module      :  MergeSort.hs
Description :  Module implements the merge sort algorithm
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Merge_sort
-}

module MergeSort where


-- | Merge sort
mergeSort :: (Ord a) => [a] -> [a]
mergeSort x
    | length x < 2 = x
    | otherwise = merge (mergeSort first) (mergeSort last)
    where
        first = take (length x `div` 2) x
        last = drop (length x `div` 2) x
        merge :: (Ord a) => [a] -> [a] -> [a]
        merge x y
            | null y = x
            | null x = y
            | head x < head y = head x : merge (tail x) y
            | otherwise = head y : merge x (tail y)


main :: IO ()
main = do
    let arr = [12,1,6,31,99,25,3,56,21,6]
    putStr "The sorted version of the array is "
    print (mergeSort arr)

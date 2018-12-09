{- |
Module      :  SelectionSort.hs
Description :  Module implements the selection sort algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Selection_sort
-}

module SelectionSort where


-- | Selection sort
selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = let x = maximum xs in selectionSort (delete x xs) ++ [x] 
    where
        delete _ [] = []
        delete y (x:xs)
            | x == y = xs
            | otherwise = x : delete y xs


main = do
    let arr = [12,1,6,31,99,25,3,56,21,6]
    putStr "The sorted version of the array is "
    print (selectionSort arr)

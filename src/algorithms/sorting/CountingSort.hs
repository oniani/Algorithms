{- |
Module      :  CountingSort.hs
Description :  Module implements the counting sort algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Counting_sort
-}

module CountingSort where

import Data.List (nub, sortBy)


-- | Counting sort
countingSort :: (Ord a) => [a] -> [a]
countingSort x = concat [replicate (snd j) (fst j) | j <- y]
    where
        y = sortBy (\ f s -> fst f `compare` fst s) (nub (zip x [length (filter (==i) x) | i <- x]))


main :: IO ()
main = do
    let arr = [12,1,6,31,99,25,3,56,21,6]
    putStr "The sorted version of the array is "
    print (countingSort arr)

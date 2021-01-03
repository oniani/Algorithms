{- |
Module      :  Shellsort.hs
Description :  Module implements the shellsort algorithm
Copyright   :  (c) David Oniani
License     :  MIT License

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Shellsort
-}

module Shellsort where

import Data.List (transpose, insert, unfoldr)


-- | Shellsort
-- Confer https://en.wikibooks.org/wiki/Algorithm_Implementation/Sorting/Shell_sort#Haskell
shellsort :: (Ord a) => [a] -> [a]
shellsort xs = foldr (decolumnize (map (foldr insert []))) xs gaps
    where
        gaps = takeWhile (< length xs) sedgewick
        sedgewick = concat [[9 * 2^n - 9 * 2^(n `div` 2) + 1, 8 * 2^(n+1) - 6 * 2^(n `div` 2) + 1] | n <- [0..]]
        decolumnize f k = concat . transpose . f . transpose . takeWhile (not . null) . unfoldr (Just . splitAt k)


main :: IO ()
main = do
    let arr = [12,1,6,31,99,25,3,56,21,6]
    putStr "The sorted version of the array is "
    print (shellsort arr)

{- |
Module      :  BubbleSort.hs
Description :  Module implements the bubble sort algorithm
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Bubble_sort
-}

module BubbleSort where


-- | Bubble sort
bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort (x:xs)
    | length (x:xs) < 2 = x:xs
    | otherwise = step (foldl go (x, []) xs)
    where
        go (y, acc) x = (min x y, max x y : acc)
        step (x, acc) = x : bubbleSort acc


main :: IO ()
main = do
    let arr = [12,1,6,31,99,25,3,56,21,6]
    putStr "The sorted version of the array is "
    print (bubbleSort arr)

{- |
Module      :  LinearSearch.hs
Description :  Module implements the linear search algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Linear_search
-}

module LinearSearch where

import qualified Data.Vector as V


-- NOTE: this implementation of the linear search is
-- a bit better than a regular one because we shrink
-- an array of possibilities 2 elements at a time
-- resulting in n/2 recursive calls. But obviously,
-- the overall algorithmic complexity is still O(n).

-- | Linear search
linearSearch :: (Ord a) => V.Vector a -> a -> Int -> Int -> Int
linearSearch x i l r
    | l > r = -1
    | x V.! l == i = l
    | x V.! r == i = r
    | otherwise = linearSearch x i (l + 1) (r - 1)


main = do
    let arr = V.fromList [1..100]
    putStr "The element with the value of 10 is at the index "
    print (linearSearch arr 10 0 99)
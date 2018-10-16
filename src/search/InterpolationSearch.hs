{- |
Module      :  InterpolationSearch.hs
Description :  Module implements the interpolation search algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Interpolation_search
-}

module InterpolationSearch where

    
-- | Interpolation search
interpolationSearch :: [Int] -> Int -> Int -> Int -> Int
interpolationSearch x i l r
    | l > r || i < x !! l || i > x !! r = -1
    | x !! pos == i = pos
    | x !! pos > i = interpolationSearch x i (l + 1) r
    | x !! pos < i = interpolationSearch x i l (r - 1)
    | otherwise = -1
    where
        pos = l + (((r - l) `div` (x !! r - x !! l)) * (i - x !! l))


main = do
    let arr = [1..100]
    putStr "The element with the value of 10 is at the index "
    print (interpolationSearch arr 10 0 99)  -- Prints out 9

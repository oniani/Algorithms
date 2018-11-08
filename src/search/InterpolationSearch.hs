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

import qualified Data.Vector as V

    
-- | Interpolation search
interpolationSearch :: V.Vector Int -> Int -> Int -> Int -> Int
interpolationSearch x i l r
    | l > r || i < x V.! l || i > x V.! r = -1
    | guess == i = pos
    | guess > i = interpolationSearch x i (l + 1) r
    | guess < i = interpolationSearch x i l (r - 1)
    | otherwise = -1
    where
        pos = l + (((r - l) `div` (x V.! r - x V.! l)) * (i - x V.! l))
        guess = x V.! pos


main = do
    let arr = V.fromList [1..100]
    putStr "The element with the value of 10 is at the index "
    print (interpolationSearch arr 10 0 99)  -- Prints out 9

{- |
Module      :  InterpolationSearch.hs
Description :  Module implements the interpolation search algorithm
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Interpolation_search
-}

module InterpolationSearch where

import Data.Vector (Vector, (!), fromList)

    
-- | Interpolation search
interpolationSearch :: Vector Integer -> Integer -> Integer -> Integer -> Integer
interpolationSearch x i l r
    | l > r || i < x ! fromInteger l || i > x ! fromInteger r = -1
    | guess == i = pos
    | guess > i = interpolationSearch x i (l + 1) r
    | guess < i = interpolationSearch x i l (r - 1)
    | otherwise = -1
    where
        pos = l + (((r - l) `div` (x ! fromInteger r - x ! fromInteger l)) * (i - x ! fromInteger l))
        guess = x ! fromInteger pos


main :: IO ()
main = do
    let arr = fromList [1..100]
    putStr "The element with the value of 10 is at the index "
    print (interpolationSearch arr 10 0 99)

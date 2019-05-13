{- |
Module      :  Bogosort.hs
Description :  Module implements the notorious bogosort algorithm
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Bogosort
-}

module Bogosort where

import System.Random (randomRIO)
import Data.List (permutations, sort)


-- | Bogosort
bogosort :: (Ord a) => [a] -> IO [a]
bogosort x
    | x == sort x = return x
    | otherwise = do
                    randomValue <- randomRIO (0, length (permutations x) - 1)
                    bogosort (permutations x !! randomValue)


main :: IO ()
main = do
    let arr = [12,1,6,31,99,25,3,56,21,6]
    arr <- bogosort arr
    putStr "The sorted version of the array is "
    print arr

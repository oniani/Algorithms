{- |
Module      :  CalkinWilfGenerator.hs
Description :  Module implements the Calkin-Wilf sequence generation algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Calkin-Wilf_tree
-}

module CalkinWilfGenerator where


-- NOTE: The Calkin-Wilf sequence is represented as the list of ordered pairs (tuples) where the
-- first element of the tuple is the numerator of the fraction with the second one being denominator.

-- | Calkin-Wilf sequence generator function
calkinWilfGenerator :: Int -> [(Int, Int)]
calkinWilfGenerator n = take n [tuplify (calkinWilfHelper i 1 [1,1]) | i <- [1..]]
    where
        tuplify [i,j] = (i,j)
        calkinWilfHelper :: Int -> Int -> [Int] -> [Int]
        calkinWilfHelper n m [i,j]
            | m == n = [i,j]
            | otherwise = calkinWilfHelper n (m + 1) [j,2 * div i j * j + j - i]


main = do
    putStr "The first 100 elements of the Calkin-Wilf sequence sequence are "
    print (calkinWilfGenerator 100)

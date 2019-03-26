{- |
Module      :  GeometricGenerator.hs
Description :  Module implements the geometric sequence generation algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
http://mathworld.wolfram.com/GeometricSequence.html
-}

module GeometricGenerator where


-- NOTE: The sequence generates the first n elements of the geometric sequence
-- Constant i stands for the initial value and the q is the common ratio.

-- | The geometric sequence generator function
geometricGenerator :: Integer -> Integer -> Integer ->[Integer]
geometricGenerator n i r = [i * r^k | k <- [0..n-1]]


main :: IO ()
main = do
    putStr "The first 100 elements of the geometric sequence are "
    print (geometricGenerator 100 1 2)

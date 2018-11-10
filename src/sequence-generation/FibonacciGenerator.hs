{- |
Module      :  FibonacciGenerator.hs
Description :  Module implements the prime number series generation algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Fibonacci_number
-}

module FibonacciGenerator where


-- | A classic Haskell implementation of the Fibonacci sequence generator
fibonacciGenerator :: Int -> [Integer]
fibonacciGenerator n = take n fiboHelper
    where
        fiboHelper :: [Integer]
        fiboHelper = 0 : 1 : zipWith (+) fiboHelper (tail fiboHelper)


main = do
    putStr "The first [lazily evaluated] 100 elements of the Fibonacci sequence are "
    print (fibonacciGenerator 100)

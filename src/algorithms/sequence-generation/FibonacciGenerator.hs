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
fibonacciGenerator :: Integer -> [Integer]
fibonacciGenerator n = take (fromInteger n) fibonacciGenerator'
    where
        fibonacciGenerator' :: [Integer]
        fibonacciGenerator' = 0 : 1 : zipWith (+) fibonacciGenerator' (tail fibonacciGenerator')


main :: IO ()
main = do
    putStr "The first 100 elements of the Fibonacci sequence are "
    print (fibonacciGenerator 100)

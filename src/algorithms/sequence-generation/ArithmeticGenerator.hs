{- |
Module      :  ArithmeticGenerator.hs
Description :  Module implements the arithmetic sequence generation algorithm
Copyright   :  (c) David Oniani
License     :  MIT License

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://www.mathsisfun.com/algebra/sequences-sums-arithmetic.html
-}

module ArithmeticGenerator where


-- NOTE: The sequence generates the first n elements of the arithmetic sequence.
-- Constant i stands for the initial value and the d is common difference.

-- | The arithmetic sequence generator function
arithmeticGenerator :: Integer -> Integer -> Integer -> [Integer]
arithmeticGenerator n i d = [i + k | k <- [0,d..d * (n-1)]]


main :: IO ()
main = do
    putStr "The first 100 elements of the arithmetic series are "
    print (arithmeticGenerator 100 10 15)

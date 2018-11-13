{- |
Module      :  PrimeGenerator.hs
Description :  Module implements the prime number series generation algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/List_of_prime_numbers
-}

module PrimeGenerator where


-- NOTE: This implementation of the prime number generator uses
-- the fact that every prime number greater than 3 is either of
-- the form 6k + 1 or 6k + 5 where k is a non-negative integer.
-- You can see the proof by elimination below.
--
-- Proof: To see why it's true, let's look at all the representations of an
-- integer (in terms of divisibility by 6). The possible representations are:
-- 6k, 6k + 1, 6k + 2, 6k + 3, 6k + 4, and 6*k + 5. Let's then consider all the cases.
-- 
-- 6k cannot be a prime since it's divisible by 6
-- 6k + 2 cannot be a prime since it's divisible by 2
-- 6k + 3 cannot be a prime since it's divisible by 3
-- 6k + 4 cannot be a prime since it's divisible by 2
--
-- Finally, we are left with 6k + 1 and 6k + 5 which are indeed the only valid
-- representations for a prime number.

-- | The prime sequence generator function
primeGenerator :: Int -> [Integer]
primeGenerator n = take n primeHelper
    where
        primeHelper :: [Integer]
        primeHelper = 2:3:primes
            where
                1:p:x = [6 * k + r | k <- [0..], r <- [1,5]]
                primes = p : filter isPrime x
                isPrime n  = all (not . divisible n) (takeWhile (\p -> p * p <= n) primes)
                divisible n p = rem n p == 0


main = do
    putStr "The first 100 primes are "
    print (primeGenerator 100)

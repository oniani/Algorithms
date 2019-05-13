{- |
Module      :  ColatzGenerator.hs
Description :  Module implements the Collatz number series generation algorithm
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Catalan_number
-}

module CollatzGenerator where


-- | The Collatz sequence generator function
collatzGenerator :: Integer -> [Integer]
collatzGenerator n = collatzGenerator' n []
    where
        collatzGenerator' :: Integer -> [Integer] -> [Integer]
        collatzGenerator' n x
            | n == 1 = x ++ [n]
            | even n = collatzGenerator' (div n 2) (x ++ [n])
            | odd n = collatzGenerator' (3 * n + 1) (x ++ [n])


main :: IO ()
main = do
    putStr "The Collatz sequence for the number 100 is "
    print (collatzGenerator 100)

{- |
Module      :  CatalanGenerator.hs
Description :  Module implements the Catalan number series generation algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Catalan_number
-}

module CatalanGenerator where


-- NOTE: The following implementation uses the fact that the nth Catalan number
-- is a product of all the numbers of the type (n + k) / k where  1 < k < 2n + 1.

-- | The Catalan sequence generator function
catalanGenerator :: Integer -> [Integer]
catalanGenerator n = take (fromIntegral n) catalanGenerator'
    where
        catalanGenerator' :: [Integer]
        catalanGenerator' = map expr [0..]
            where
                expr n = product [n+2..2*n] `div` product [2..n]


main = do
    putStr "The first 100 elements of the Catalan sequence are "
    print (catalanGenerator 100)

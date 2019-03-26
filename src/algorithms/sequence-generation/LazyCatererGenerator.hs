{- |
Module      :  LazyCatererGenerator.hs
Description :  Module implements the lazy caterer's sequence generation algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Lazy_caterer%27s_sequence
-}

module LazyCatererGenerator where


-- | Lazy caterer's sequence generator function
lazyCatererGenerator :: Integer -> [Integer]
lazyCatererGenerator x = take (fromInteger x) [div (i^2 + i + 2) 2 | i <- [0..]]


main :: IO ()
main = do
    putStr "The first 100 elements of the lazy caterer's sequence are "
    print (lazyCatererGenerator 100)

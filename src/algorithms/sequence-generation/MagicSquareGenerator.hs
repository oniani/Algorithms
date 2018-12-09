{- |
Module      :  MagicSquareGenerator.hs
Description :  Module implements the magic square series generation algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
http://number-cube.com/portfolio/magic-square/
-}

module MagicSquareGenerator where


-- | Recaman square series generator
magicSquareGenerator :: Integer -> [Integer]
magicSquareGenerator x = take (fromInteger x) [div (i * (i^2 + 1)) 2 | i <- [3..]]


main = do
    putStr "The first 100 elements of the magic square series are "
    print (magicSquareGenerator 100)

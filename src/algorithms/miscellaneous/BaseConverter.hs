{- |
Module      :  BaseConverter.hs
Description :  Module implements the base conversion algorithm
Copyright   :  (c) David Oniani
License     :  GNU General Public License v3.0

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Change_of_base
-}

module BaseConverter where


-- | A function that converts an integer to a string in any base
baseConverter :: Integer -> Integer -> String
baseConverter n b
    | n < b = [s !! fromInteger n]
    | otherwise = baseConverter (div n b) b ++ [s !! fromInteger (rem n b)]
    where
        s = "0123456789ABCDEF"


main :: IO ()
main = do
    putStr "The result of the conversion is "
    print (baseConverter 3735928559 16)

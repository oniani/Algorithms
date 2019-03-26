{- |
Module      :  RecamanGenerator.hs
Description :  Module implements the Recaman sequence generation algorithm
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
http://oeis.org/A005132
-}

module RecamanGenerator where


-- | Recaman sequence generator function
recamanGenerator :: Integer -> [Integer]
recamanGenerator = recamanGenerator' [0]
    where
        recamanGenerator' :: [Integer] -> Integer -> [Integer]
        recamanGenerator' x n
            | n == 0 = x
            | expr1 > 0 && notElem expr1 x = recamanGenerator' (x ++ [expr1]) (n - 1)
            | otherwise = recamanGenerator' (x ++ [expr2]) (n - 1)
            where
                expr1 = last x - toInteger (length x)
                expr2 = last x + toInteger (length x)


main :: IO ()
main = do
    putStr "The first 100 elements of the Recaman's sequence are "
    print (recamanGenerator 100)

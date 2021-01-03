{- |
Module      :  DiatomicGenerator.hs
Description :  Module implements the Stern's diatomic sequence generation algorithm
Copyright   :  (c) David Oniani
License     :  MIT License

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
http://www-math.mit.edu/~rstan/papers/stern.pdf
-}

module DiatomicGenerator where


-- | Stern's diatomic sequence generator function
diatomicGenerator :: Integer -> [Integer]
diatomicGenerator n = take (fromInteger n) [diatomicGenerator' i | i <- [0..]]
    where
        diatomicGenerator' :: Integer -> Integer
        diatomicGenerator' k
            | k < 2 = k
            | even k = diatomicGenerator' (div k 2)
            | odd k = diatomicGenerator' (div (k - 1) 2) + diatomicGenerator' (div (k + 1) 2)


main :: IO ()
main = do
    putStr "The first 100 elements of the Stern's diatomic sequence are "
    print (diatomicGenerator 100)

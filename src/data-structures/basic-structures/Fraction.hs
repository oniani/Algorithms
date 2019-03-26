{- |
Module      :  Fraction.hs
Description :  Module implements the Fraction datatype
Copyright   :  (c) David Oniani
License     :  MIT
Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Rational_number
-}

module Fraction
    ( Fraction (..)
    , numerator
    , denominator
    , reduce
    , plus
    , minus
    , times
    , over
    , (+)
    , (-)
    , (*)
    , abs
    , (/)
    ) where

type Numerator   = Integer
type Denominator = Integer
data Fraction    = Fraction Numerator Denominator deriving (Eq)

-- Show the Fraction number
instance Show Fraction
    where
        show (Fraction numerator denominator) = show numerator ++ "/" ++ show denominator


numerator :: Fraction -> Numerator
numerator (Fraction numerator _) = numerator

denominator :: Fraction -> Numerator
denominator (Fraction _ denominator) = denominator

reduce :: Fraction -> Fraction
reduce (Fraction numerator denominator) = Fraction reducedNumerator reducedDenominator
    where
        reducedNumerator = numerator `div` greatestCommonDivisor
        reducedDenominator = denominator `div` greatestCommonDivisor
        greatestCommonDivisor = gcd numerator denominator

plus :: Fraction -> Fraction -> Fraction
plus (Fraction numerator1 denominator1) (Fraction numerator2 denominator2) = reducedFraction
    where
        numerator3 = numerator1 * denominator2 + numerator2 * denominator1
        denominator3 = denominator1 * denominator2
        reducedFraction = reduce (Fraction numerator3 denominator3)

minus :: Fraction -> Fraction -> Fraction
minus (Fraction numerator1 denominator1) (Fraction numerator2 denominator2) = reducedFraction
    where
        numerator3 = numerator1 * denominator2 - numerator2 * denominator1
        denominator3 = denominator1 * denominator2
        reducedFraction = reduce (Fraction numerator3 denominator3)

times :: Fraction -> Fraction -> Fraction
times (Fraction numerator1 denominator1) (Fraction numerator2 denominator2) = reducedFraction
    where
        numerator3 = numerator1 * numerator2
        denominator3 = denominator1 * denominator2
        reducedFraction = reduce (Fraction numerator3 denominator3)

over :: Fraction -> Fraction -> Fraction
over (Fraction numerator1 denominator1) (Fraction numerator2 denominator2) = reducedFraction
    where
        numerator3 = numerator1 * denominator2
        denominator3 = numerator2 * denominator1
        reducedFraction = reduce (Fraction numerator3 denominator3)

instance Num Fraction where
   Fraction n1 d1 + Fraction n2 d2 = Fraction n1 d1 `plus`  Fraction n2 d2
   Fraction n1 d1 - Fraction n2 d2 = Fraction n1 d1 `minus` Fraction n2 d2
   Fraction n1 d1 * Fraction n2 d2 = Fraction n1 d1 `times` Fraction n2 d2
   abs           (Fraction n d)    = Fraction (abs n) (abs d)
   signum                          = undefined
   fromInteger                     = undefined

instance Fractional Fraction where
    Fraction n1 d1 / Fraction n2 d2 = Fraction n1 d1 `over` Fraction n2 d2
    fromRational                    = undefined


main :: IO ()
main = do
    let fraction1 = Fraction 1 3
    let fraction2 = Fraction 1 3
    let fraction3 = Fraction 8 3
    print fraction1
    print fraction2
    print fraction3
    putStr (show fraction1 ++ " + " ++ show fraction2 ++ " = ")
    print (fraction1 + fraction2)
    putStr (show fraction3 ++ " - " ++ show fraction1 ++ " = ")
    print (fraction3 - fraction1)
    putStr (show fraction2 ++ " * " ++ show fraction3 ++ " = ")
    print (fraction2 * fraction3)
    putStr (show fraction2 ++ " / " ++ show fraction3 ++ " = ")
    print (fraction2 / fraction3)

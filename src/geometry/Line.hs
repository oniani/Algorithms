{- |
Module      :  Line.hs
Description :  Module implements various algorithms to deal with lines in XOY coordinate system.
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Linear_function_(calculus)
-}


module Line
    ( Line (..)
    , findInitialValue
    , findSlope
    , findFunction
    , isParallel
    , intersect
    ) where


newtype Point = Point (Double, Double) deriving (Eq, Show)
newtype Line = Line [Point] deriving (Eq, Show)


-- | Find the initial value of the linear function
findInitialValue :: Line -> Double
findInitialValue (Line [Point (x1,y1), Point (x2,y2)]) = (y1 * x2 - y2 * x1) / (x2 - x1)

-- -- | Find a the slope of the linear function
findSlope :: Line -> Double
findSlope (Line [Point (x1,y1), Point (x2,y2)]) = (y2 - y1) / (x2 - x1)

-- -- | Finds a linear function which goes through the given line
findFunction :: Line -> String
findFunction (Line a) = "f(x) = " ++ show k ++ "x + " ++ show b
    where
        b = findInitialValue (Line a)
        k = findSlope (Line a)

-- -- | Checks if two lines are parallel to each other
isParallel :: Line -> Line -> Bool
isParallel (Line a) (Line b) = findSlope (Line a) == findSlope (Line b)

-- -- | Checks if two lines intersect each other and returns the intersection point if they do
intersect :: Line -> Line -> Maybe Point
intersect (Line a) (Line b)
    | findSlope (Line a) == findSlope (Line b) = Nothing
    | otherwise = Just (Point (v1 / v2, v5 / v2))
    where
        v1 = findInitialValue (Line b) - findInitialValue (Line a)
        v2 = findSlope (Line a) - findSlope (Line b)
        v3 = findSlope (Line a) * findInitialValue (Line b)
        v4 = findInitialValue (Line a) * findInitialValue (Line a)
        v5 = v3 - v4


main = do
    let line1 = Line [Point (2, 4.5), Point (4, 8)]
    let line2 = Line [Point (1, 1), Point (2, 2)]
    let line3 = Line [Point (1, 4), Point (-3, 0)]

    print (findInitialValue line1)
    print (findSlope line1)
    print (findFunction line1)
    putStr "\n"
    print (findInitialValue line2)
    print (findSlope line2)
    print (findFunction line2)
    putStr "\n"
    print (findInitialValue line3)
    print (findSlope line3)
    print (findFunction line3)
    putStr "\n"
    print (isParallel line1 line2)
    print (isParallel line2 line3)
    print (isParallel line1 line3)
    putStr "\n"
    print (line1 `intersect` line2)
    print (line2 `intersect` line3)
    print (line1 `intersect` line3)

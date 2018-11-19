{- |
Module      :  Triangle.hs
Description :  Module implements various algorithms to deal with triangles.
Copyright   :  (c) David Oniani
License     :  MIT

Maintainer  :  onianidavid@gmail.com
Stability   :  provisional
Portability :  portable

For more information, follow the link below.
https://en.wikipedia.org/wiki/Triangle
-}


module Triangle
    ( Triangle (..)
    , toDegreesTriangle
    , format
    , formatMore
    , formatMany
    , triangleExists
    , perimeter
    , area
    , bisectors
    , heights
    , medians
    , inradius
    , circumradius
    , exradii
    , sines
    , cosines
    , angles
    , info
    ) where


data Triangle = Triangle Double Double Double deriving (Eq)


-- | Show the triangle
instance Show Triangle
    where
        show (Triangle a b c)
            | triangleExists tri = tp ++ ab ++ bc ++ ca
            where
                tri = Triangle a b c
                tp = "Triangle ABC: "
                ab = "AB = " ++ show a ++ "  "
                bc = "BC = " ++ show b ++ "  "
                ca = "CA = " ++ show c

-- | Helper function to convert a list of three angles in radians into a list of 3 angles in degrees
toDegreesTriangle :: [Double] -> [Double]
toDegreesTriangle [a,b,c] = [a * 180 / pi,b * 180 / pi,c * 180 / pi]

-- | Helper function to format a list
format :: [Double] -> String -> String -> String -> String
format x fs st ts = str0 ++ str1 ++ str2
    where
        str0 = fs ++ " = " ++ show (x !! 0) ++ ", "
        str1 = st ++ " = " ++ show (x !! 1) ++ ", "
        str2 = ts ++ " = " ++ show (x !! 2)

-- | Helper function to format a list of tuples
formatMore :: [(Double,Double)] -> String -> String -> String -> String
formatMore x fc sc tc = str0 ++ str1 ++ str2
    where
        str0 = fc ++ " " ++ show (x !! 0) ++ ", "
        str1 = sc ++ " " ++ show (x !! 1) ++ ", "
        str2 = tc ++ " " ++ show (x !! 2)

-- | Helper function to format a list of lists of tuples
formatMany :: [[(Double,Double)]] -> String -> String -> String -> String
formatMany x fc sc tc = str0 ++ str1 ++ str2
    where
        str0 = fc ++ " " ++ show (x !! 0) ++ ", "
        str1 = sc ++ " " ++ show (x !! 1) ++ ", "
        str2 = tc ++ " " ++ show (x !! 2)


-- | Restrictions (triangle inequalities)
triangleExists :: Triangle -> Bool
triangleExists (Triangle a b c)
    | b + c > a && a + c > b && a + b > c && a > 0 && b > 0 && c > 0 = True
    | otherwise = error "Such triangle does not exist!"

-- | Perimeter of the triangle
perimeter :: Triangle -> Double
perimeter (Triangle a b c)
    | triangleExists tri = a + b + c
    where
        tri = Triangle a b c

-- | Area of the triangle (heron's formula)
area :: Triangle -> Double
area (Triangle a b c)
    | triangleExists tri = sqrt (s * (s - a) * (s - b) * (s - c)) 
    where
        tri = Triangle a b c
        s   = (a + b + c)  /  2

-- | Bisectors of the triangle
bisectors :: Triangle -> [Double]
bisectors (Triangle a b c)
    | triangleExists tri = [biA,biB,biC]
    where
        tri = Triangle a b c
        biA = sqrt (b * c * s * (s - 2 * a)) / (s - a)
        biB = sqrt (a * c * s * (s - 2 * b)) / (s - b)
        biC = sqrt (a * b * s * (s - 2 * c)) / (s - c)
        s   = a + b + c

-- | Heights of the triangle
heights :: Triangle -> [Double]
heights (Triangle a b c)
    | triangleExists tri = [a2X / a,a2X / b,a2X / c]
    where
        tri = Triangle a b c
        a2X = 2 * area tri

-- | Medians of the triangle (Apollonius' theorem)
medians :: Triangle -> [Double]
medians (Triangle a b c)
    | triangleExists tri = [meA,meB,meC]
    where
        tri = Triangle a b c
        meA = sqrt (exp - 3 * a^2) / 2
        meB = sqrt (exp - 3 * b^2) / 2
        meC = sqrt (exp - 3 * c^2) / 2
        exp = 2 * a^2 + 2 * b^2 + 2 * c^2

-- | Inradius of the triangle
inradius :: Triangle -> Double
inradius (Triangle a b c)
    | triangleExists tri = a2X / per
    where 
        tri = Triangle a b c
        a2X = 2 * area tri
        per = perimeter tri

-- | Circumradius of the triangle
circumradius :: Triangle -> Double
circumradius (Triangle a b c)
    | triangleExists tri = (a * b * c) / a4X
    where
        tri = Triangle a b c
        a4X = 4 * area tri

-- | Find the exradii of the triangle
exradii :: Triangle -> [Double]
exradii (Triangle a b c)
    | triangleExists tri = [rA,rB,rC]
    where
        tri = Triangle a b c
        rA  = sqrt ((s * (s - b) * (s - c)) / (s - a))
        rB  = sqrt ((s * (s - a) * (s - c)) / (s - b))
        rC  = sqrt ((s * (s - a) * (s - b)) / (s - c))
        s   = (a + b + c) / 2

-- | Sines of the triangle
sines :: Triangle -> [Double]
sines (Triangle a b c)
    | triangleExists tri = [snA,snB,snC]
    where
        tri = Triangle a b c
        snA = a / (2 * circumradius tri)
        snB = b / (2 * circumradius tri)
        snC = c / (2 * circumradius tri)

-- | Cosines of the triangle (using the law of cosines)
cosines :: Triangle -> [Double]
cosines (Triangle a b c)
    | triangleExists tri = [coA,coB,coC]
    where
        tri = Triangle a b c
        coA = (b^2 + c^2 - a^2) / (2 * b * c)
        coB = (a^2 + c^2 - b^2) / (2 * a * c)
        coC = (a^2 + b^2 - c^2) / (2 * a * b)

-- | Sines of the triangle (using the law of sines)
angles :: Triangle -> [Double]
angles (Triangle a b c)
    | triangleExists tri = [anA,anB,anC]
    where
        tri = Triangle a b c
        anA = asin (sines tri !! 0)
        anB = asin (sines tri !! 1)
        anC = asin (sines tri !! 2)

-- | Display all the information about the triangle
info :: Triangle -> IO()
info (Triangle a b c)
    | triangleExists tri = putStr (sta ++ des ++ end ++
                                   per ++ are ++ bis ++
                                   hei ++ med ++ inr ++
                                   cir ++ exr ++ sns ++
                                   css ++ agr ++ agd)
    where
        tri = Triangle a b c
        sta = "--------------------------------------------------"                                                   ++ "\n"
        des = show tri                                                                                               ++ "\n"
        end = "--------------------------------------------------"                                                   ++ "\n"
        per = "Perimeter:          " ++ show       (perimeter                  tri)                                  ++ "\n"
        are = "Area:               " ++ show       (area                       tri)                                  ++ "\n"
        bis = "Bisectors:          " ++ format     (bisectors                  tri) "AD" "BE" "CF"                   ++ "\n"
        hei = "Heights:            " ++ format     (heights                    tri) "AG" "BH" "CI"                   ++ "\n"
        med = "Medians:            " ++ format     (medians                    tri) "AJ" "BK" "CL"                   ++ "\n"
        inr = "Inradius:           " ++ show       (inradius                   tri)                                  ++ "\n"
        cir = "Circumradius:       " ++ show       (circumradius               tri)                                  ++ "\n"
        exr = "Exradii:            " ++ show       (exradii                    tri)                                  ++ "\n"
        sns = "Sines:              " ++ format     (sines                      tri) "sin <BAC" "sin <ABC" "sin <ACB" ++ "\n"
        css = "Cosines:            " ++ format     (cosines                    tri) "cos <BAC" "cos <ABC" "cos <ACB" ++ "\n"
        agr = "Angles (radians):   " ++ format     (angles                     tri) "< BAC" "< ABC" "< ACB"          ++ "\n"
        agd = "Angles (degrees):   " ++ format     (toDegreesTriangle $ angles tri) "< BAC" "< ABC" "< ACB"          ++ "\n"


main = do
    let triangle = Triangle 3 4 5
    info triangle

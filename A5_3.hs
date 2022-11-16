{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

data Point = MkPoint Float Float deriving (Eq, Ord, Show)
data Vector = MkVector Float Float deriving (Eq, Ord, Show)
data Triangle = MkTriangle Point Point Point
data Quad = MkQuad Point Point Point Point

{-

class Polygon p where
    area            :: Polygon p => p -> Float
    translate_poly  :: Polygon p => p -> Vector -> p
    scale_poly      :: Polygon p => p -> Float -> p
-}

tt :: Triangle
tt = MkTriangle x1 x2 x3

translate :: Point -> Vector -> Point
translate (MkPoint p1 p2) (MkVector v1 v2) = MkPoint (p1+v1) (p2+v2)

scale :: Float -> Point -> Point
scale factor (MkPoint p1 p2) = MkPoint (factor * p1) (factor * p2)

x1 :: Point
x1 = MkPoint 2 2

x2 :: Point
x2 = MkPoint 8 2

x3 :: Point
x3 = MkPoint 7 5

lengthBetweenTwoPoints :: Point -> Point -> Float
lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x2 y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

findGround :: Point -> Point -> Point -> Float
findGround (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3) 
    | (lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x2 y2) > lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x3 y3)) || (lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x2 y2) > lengthBetweenTwoPoints (MkPoint x2 y2) (MkPoint x3 y3)) = lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x2 y2)
    | (lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x3 y3) > lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x2 y2)) || (lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x3 y3) > lengthBetweenTwoPoints (MkPoint x2 y2) (MkPoint x3 y3)) = lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x3 y3)
    | otherwise = lengthBetweenTwoPoints (MkPoint x2 y2) (MkPoint x3 y3)

calculateHalfOfSumEdge :: Point -> Point -> Point -> Float
calculateHalfOfSumEdge (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3) = 0.5 * (lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x2 y2) + lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x3 y3) +lengthBetweenTwoPoints (MkPoint x2 y2) (MkPoint x3 y3))

calculateHeight :: Point -> Point -> Point -> Float
calculateHeight (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3) = (2/findGround (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3))*sqrt(calculateHalfOfSumEdge (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3)*(calculateHalfOfSumEdge (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3)-lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x2 y2))*(calculateHalfOfSumEdge (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3)-lengthBetweenTwoPoints (MkPoint x1 y1) (MkPoint x3 y3))*(calculateHalfOfSumEdge (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3)-lengthBetweenTwoPoints (MkPoint x2 y2) (MkPoint x3 y3)))

areatri :: Point -> Point -> Point -> Float
areatri (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3) = 0.5* findGround (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3) * calculateHeight (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3)

area :: Triangle-> Float
area (MkTriangle(MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3)) = 0.5* findGround (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3) * calculateHeight (MkPoint x1 y1) (MkPoint x2 y2) (MkPoint x3 y3)
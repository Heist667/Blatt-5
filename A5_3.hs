{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

data Point = MkPoint {
    px :: Float,
    py :: Float
} deriving (Eq, Ord, Show)

data Vector = MkVector {
    vx :: Float,
    vy :: Float
}deriving (Eq, Ord, Show)
{-}
data Triangle = MkTriangle Point Point Point
data Quad = MkQuad Point Point Point Point

class Polygon p where
    area            :: Polygon p => p -> Float
    translate_poly  :: Polygon p => p -> Vector -> p
    scale_poly      :: Polygon p => p -> Float -> p
-}

tt :: Triangle
tt = MkTriangle x1 x2 x3
{-}
translate :: Point -> Vector -> Point
translate (MkPoint p1 p2) (MkVector v1 v2) = MkPoint (p1+v1) (p2+v2)

scale :: Float -> Point -> Point
scale factor (MkPoint p1 p2) = MkPoint (factor * p1) (factor * p2)
-}
x1 :: Point
x1 = MkPoint 2 2

x2 :: Point
x2 = MkPoint 8 2

x3 :: Point
x3 = MkPoint 7 5

v1:: Vector
v1 = MkVector 2 2

getXfromPoint :: Point -> Float
getXfromPoint (MkPoint px py) = px

getYfromPoint :: Point -> Float
getYfromPoint (MkPoint px py) = py

getXfromVector :: Vector -> Float
getXfromVector (MkVector vx vy) = vx

getYfromVector :: Vector -> Float
getYfromVector (MkVector vx vy) = vy

translate :: Point -> Vector -> Point
translate p v = MkPoint (getXfromPoint p + getXfromVector v) (getYfromPoint p + getYfromVector v)

scale :: Point -> Float -> Point
scale p f = MkPoint (f*getXfromPoint p) (f*getYfromPoint p)

lengthBetweenTwoPoints :: Point -> Point -> Float
lengthBetweenTwoPoints p1 p2 = sqrt((getXfromPoint p1-getXfromPoint p2)^2 + (getYfromPoint p1- getYfromPoint p2)^2)

data Triangle = MkTriangle {
    p1 :: Point,
    p2 :: Point,
    p3 :: Point
}

{-
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
-}
module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectangleArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea b c * 2

-- The use of this function is silly, but illustrates how functions may not be
-- exported by a module
rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b

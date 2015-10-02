module Sphere (
    Sphere(..),
    intersects
    ) where

import Vector
import Ray

import Data.List

data Sphere = Sphere {
    center :: Vector,
    radius :: Double
    } deriving (Show)
         
instance Eq Sphere where
    s1 == s2 = (radius s1 == radius s2) && (center s1 == center s2)

intersects :: Sphere -> Ray -> Maybe Vector
intersects sphere ray =
    let
        -- An optimisation: A dot product of a vector with itself
        -- is equal to the length of the vector squared. And 
        -- since a Ray.direction is a UnitVector, the length
        -- squared is just a hard coded 1
        -- a = dotProduct (direction ray) (direction ray)
        a = 1
        oMinusC = (origin ray) - (center sphere)
        b = 2 * dotProduct oMinusC (direction ray)
        -- Another dot product of a vector with itself
        -- c = (dotProduct oMinusC oMinusC) - (radius sphere ** 2)
        sphereRadius = radius sphere
        c = (lengthSquared oMinusC) - (sphereRadius * sphereRadius)
        quad = quadraticE a b c
    in
        case quad of
        Nothing -> Nothing
        Just (plus,minus) ->
            let
                sorted = sort [x | x <- [plus,minus], x > 0]
            in
                case sorted of
                [] -> Nothing
                _ -> Just ((origin ray) + (Vector.multiply (direction ray) (head sorted)))

quadraticE :: Double -> Double -> Double -> Maybe (Double,Double)
quadraticE a b c =
    let
        discriminant = b*b - 4*a*c
    in
        if discriminant < 0
        then Nothing
        else
            let
                sqrtDiscriminant = sqrt discriminant
            in
                Just (((-b) + (sqrtDiscriminant)) / 2.0,
                      ((-b) - (sqrtDiscriminant)) / 2.0)

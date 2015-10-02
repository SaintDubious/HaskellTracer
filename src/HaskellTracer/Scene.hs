module Scene (
    Scene(..),
    readScene,
    Scene.intersects
    ) where

import Sphere
import Vector
import Ray
import Color
import Light

import Control.Exception
import System.IO
import Control.Monad
import Data.Monoid
import Data.Maybe
import Data.List
import Data.Word

data Scene = Scene {
    spheres :: [Sphere],
    lights :: [Light]
    } deriving (Show)
               
readScene :: FilePath -> IO Scene
readScene fileName = do
    contents <- liftM lines (readFile fileName)
    (spheres,lights) <- return $ readLines contents
    return (Scene spheres lights)

readLines :: [String] -> ([Sphere],[Light])
readLines [] = ([],[])
readLines (l:lns) =
    let
        tokens = words l
    in
        case head tokens of
            "SPHERE" -> 
                let
                    thisSphere = Sphere (vector (read (tokens!!2) :: Double)
                                                (read (tokens!!3) :: Double)
                                                (read (tokens!!4) :: Double))
                                        (read (tokens!!1) :: Double)
                    (spheres,lights) = (readLines lns)
                in
                    ((thisSphere : spheres), lights)
            "LIGHT" ->
                let
                    thisLights = Light (read (tokens!!1) :: Double)
                                             (read (tokens!!2) :: Double)
                                             (read (tokens!!3) :: Double)
                    (spheres,lights) = (readLines lns)
                in
                    (spheres, (thisLights : lights))

intersects :: Scene -> Ray -> Maybe Sphere -> Integer -> Color
intersects _ _ _ 0 = black
intersects scene ray skipSphere depth =
    let 
        results = intersectionTest scene ray skipSphere
    in
        case results of
        Nothing -> black
--        Just (s, v) -> rayColor scene s ray v
        Just (s, v) -> 
            let
                lightPoint = position $ head $ lights scene
                rayToLight = Ray v (normalize (lightPoint - v))
                shadow = intersectionTest scene rayToLight (Just s)
            in
                case shadow of
                Nothing -> rayColor scene s ray v depth
                _ -> black

intersectionTest :: Scene -> Ray -> Maybe Sphere -> Maybe (Sphere, Vector)
intersectionTest scene ray skipSphere = 
    let 
        results = map (\sphere -> (sphere, Sphere.intersects sphere ray)) (spheres scene)
        justVectors = filter (\(sphere, vector) -> ((isJust vector) && (Just sphere /= skipSphere))) results
    in
        if null justVectors
        then Nothing
        else
            let
                points = minimumBy (\(s1, Just a) (s2, Just b) -> compare (lengthSquared a) (lengthSquared b)) justVectors
            in
                Just ((fst points), fromJust (snd points))


rayColor :: Scene -> Sphere -> Ray -> Vector -> Integer -> Color
rayColor scene sphere ray intersection depth =
    let
        pointNormal = normalize (intersection - (center sphere))
        firstLight = head (lights scene)
        lightVector = normalize ((position firstLight) - intersection)
        diffuse = max 0.0 (dotProduct lightVector pointNormal)
        reflectRay = Ray intersection (reflection lightVector pointNormal)
        specular = max 0.0 ((dotProduct (direction ray) (direction reflectRay))) ** 100

        incomingRayReflection = Ray intersection (reflection (direction ray) pointNormal)
        reflectionColor = Scene.intersects scene incomingRayReflection (Just sphere) (depth - 1)



        intensity = diffuse*210.0 + specular*200.0 + (fromIntegral(r reflectionColor)::Double) / 10.0
    in
        Color (truncate (min 255 intensity))
              (truncate (min 255 intensity))
              (truncate (min 255 intensity))

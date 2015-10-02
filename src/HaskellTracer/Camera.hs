module Camera where

import Scene
import Color
import Ray
import Vector

import Data.Traversable
import Data.Word
--import Control.Parallel.Strategies

data Camera = Camera {
    focalLength :: Int,
    width :: Int,
    height :: Int
    } deriving (Show)

captureImage :: Camera -> Scene -> [[Color]]              
captureImage camera scene =
    let
        yRange = [(1 + (-height camera) `quot` 2) .. (height camera `quot` 2)]
    in
        map (\y ->
                 captureRow camera scene y
            ) $ reverse yRange


captureRow :: Camera -> Scene -> Int -> [Color]
captureRow camera scene y =
    let
        xRange = [(1 + (-width camera) `quot` 2) .. (width camera `quot` 2)]
    in
        map (\x ->
                 let
                     ray = Ray (vector 0.0 0.0 (fromIntegral (-(focalLength camera)) :: Double))
                               (unitVector (fromIntegral x :: Double)
                                           (fromIntegral y :: Double)
                                           (fromIntegral (focalLength camera) :: Double))
                 in
                     intersects scene ray Nothing 3
            ) xRange
        

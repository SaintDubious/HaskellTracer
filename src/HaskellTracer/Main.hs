module Main where

import Vector
import Bitmap
import Color
import Scene
import Camera

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Text.Printf

main :: IO ()
main = do
    scene <- readScene "busy.txt"
    putStrLn (show scene)
    let camera = Camera 500 600 600

    let image = captureImage camera scene
    
    putStrLn ("height is " ++ show(Prelude.length image))
    putStrLn ("width is " ++ show(Prelude.length (head image)))
    BL.writeFile "test.bmp" $ runPut (bmp image)
       

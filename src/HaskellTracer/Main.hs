module Main where

import Vector
import Bitmap
import Color
import Scene
import Camera

import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Text.Printf
import Data.Time.Clock
import Control.Parallel
import Control.DeepSeq

main :: IO ()
main = do
    scene <- readScene "busy.txt"
    putStrLn (show scene)
    let camera = Camera 500 600 600

    start <- getCurrentTime
    let image = captureImage camera scene
--    let image = map (\y -> (map (\x -> black) [0..599])) [0..599]
    forceImage image
    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."

    putStrLn ("height is " ++ show(Prelude.length image))
    putStrLn ("width is " ++ show(Prelude.length (head image)))
    BL.writeFile "test.bmp" $ runPut (bmp image)
       
forceImage :: [[Color]] -> IO ()
forceImage (x:xs) = rnf x `pseq` forceImage xs
forceImage _ = return ()


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
import Control.Parallel.Strategies
import Control.DeepSeq

main :: IO ()
main = do
    scene <- readScene "busy.txt"
    putStrLn (show scene)
    let camera = Camera 500 600 600

    start <- getCurrentTime
    let image = forceImage $ captureImage camera scene

    putStrLn ("height is " ++ show(Prelude.length image))
    putStrLn ("width is " ++ show(Prelude.length (head image)))

    end <- getCurrentTime
    putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."

    imgStart <- getCurrentTime
    BL.writeFile "test.bmp" $ runPut (bmp image)
    imgEnd <- getCurrentTime
    putStrLn $ show (imgEnd `diffUTCTime` imgStart) ++ " elapsed."

forceImage (x:xs) = runEval $ do
        xs' <- rpar $ forceImage xs
        rseq $ force x
        rseq xs'
        return (x : xs')
forceImage [] = []

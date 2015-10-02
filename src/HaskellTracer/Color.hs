module Color where

import Data.Word

data Color = Color {
    r :: Word8,
    g :: Word8,
    b :: Word8
    } deriving (Show)
             
red = Color 255 0 0
green = Color 0 255 0
blue = Color 0 0 255
black = Color 0 0 0
white = Color 255 255 255

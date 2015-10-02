module Color where

import Data.Word
import Control.Parallel
import Control.DeepSeq

data Color = Color {
    r :: Word8,
    g :: Word8,
    b :: Word8
    } deriving (Show)
    
instance NFData Color where
    rnf (Color r g b) = (r,g,b) `pseq` ()
--    rnf (Color r g b) = ()
--    rnf _ = ()

red = Color 255 0 0
green = Color 0 255 0
blue = Color 0 0 255
black = Color 0 0 0
white = Color 255 255 255

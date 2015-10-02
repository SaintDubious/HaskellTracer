module Bitmap (bmp) where

import Color

import System.IO
import Data.Binary.Put
import Data.Word
import Control.Monad
import qualified Data.ByteString.Char8 as C

bytesPerPlane = 3
bitsPerPlane = bytesPerPlane*8

bmp :: [[Color]] -> Put
bmp colors = do
    bmpFileHeader (54 + ((height colors) * (paddedWidthInBytes colors)))
    bmpInfoHeader (width colors) (height colors)
    bmpColors colors

bmpFileHeader :: Word32 -> Put
bmpFileHeader s = do
    putByteString (C.pack "BM")
    putWord32le s
    putWord32le 0
    putWord32le 54 -- sizeof(bmpFileHeader) + sizeof(BitmapInfoHeader)
                  
bmpInfoHeader :: Word32 -> Word32 -> Put
bmpInfoHeader w h = do
    putWord32le 40 -- sizeof(BitmapInfoHeader)
    putWord32le w
    putWord32le h
    putWord16le 1
    putWord16le bitsPerPlane
    putWord32le 0
    putWord32le 0
    putWord32le 0
    putWord32le 0
    putWord32le 0
    putWord32le 0                
                
bmpColors :: [[Color]] -> Put
bmpColors [] = return()
bmpColors colors =
    let
        rowWidthInBytes = (width colors) * fromIntegral(bytesPerPlane) :: Word32
        pad = (paddedWidthInBytes colors) - rowWidthInBytes
    in do
        bmpRow (last colors) pad
        bmpColors (init colors) 

bmpRow :: [Color] -> Word32 -> Put
bmpRow (c:clrs) pad = do
    putWord8 (b c)
    putWord8 (g c)
    putWord8 (r c)
    bmpRow clrs pad
bmpRow [] 0 = return()   
bmpRow [] n = do
    putWord8 0
    bmpRow [] (n-1)
  
width :: [[Color]] -> Word32
width colors = fromIntegral(length (head colors)) :: Word32

height :: [[Color]] -> Word32
height colors = fromIntegral(length colors) :: Word32

paddedWidthInBytes :: [[Color]] -> Word32
paddedWidthInBytes colors =
    let
        colorsWidth = (width colors)
    in     
        3*colorsWidth + (mod (3*colorsWidth) 4)


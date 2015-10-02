module Light where

import Vector

data Light = Light Double Double Double
           deriving (Show)
             
position :: Light -> Vector
position (Light x y z) = vector x y z

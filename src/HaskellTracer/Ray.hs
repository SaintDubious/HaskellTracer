module Ray where

import Vector

-- Note:
-- That direction actually needs to be UnitVector
-- but as of yet I don't know how to export the
-- UnitVector type without the Data Constuctor
data Ray = Ray {
    origin :: Vector,
    direction :: Vector
    } deriving (Show)
           
    

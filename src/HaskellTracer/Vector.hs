module Vector (
    Vector,
    vector,
    unitVector,
    lengthSquared,
    Vector.length,
    dotProduct,
    multiply,
    normalize,
    reflection
    ) where

data Vector = Vector Double Double Double
              | UnitVector Double Double Double
                deriving (Show)

x :: Vector -> Double
x (Vector x _ _) = x
x (UnitVector x _ _) = x

y :: Vector -> Double
y (Vector _ y _) = y
y (UnitVector _ y _) = y

z :: Vector -> Double
z (Vector _ _ z) = z
z (UnitVector _ _ z) = z

instance Num Vector where
    (Vector x1 y1 z1) - (Vector x2 y2 z2) = Vector (x1 - x2) (y1 - y2) (z1 - z2)
    (Vector x1 y1 z1) + (Vector x2 y2 z2) = Vector (x1 + x2) (y1 + y2) (z1 + z2)
    Vector {} * Vector {} = undefined
    abs Vector {} = undefined
    signum Vector {} = undefined
    fromInteger a = undefined

instance Eq Vector where
    v1 == v2 = (x v1 == x v2) && (y v1 == y v2) && (z v1 == z v2)

vector :: Double -> Double -> Double -> Vector
vector x y z = Vector x y z

unitVector :: Double -> Double -> Double -> Vector
unitVector x y z =
    let
        l = Vector.length $ vector x y z
    in
        UnitVector (x/l) (y/l) (z/l)
        
normalize :: Vector -> Vector
normalize (Vector x y z) = unitVector x y z

lengthSquared :: Vector -> Double
lengthSquared (UnitVector _ _ _) = 1.0
lengthSquared (Vector x y z) = x*x + y*y + z*z

length :: Vector -> Double
length (UnitVector _ _ _) = 1.0
length v = sqrt $ lengthSquared v

dotProduct :: Vector -> Vector -> Double
dotProduct v1 v2 = x v1 * x v2 + y v1 * y v2 + z v1 * z v2

multiply :: Vector -> Double -> Vector
multiply v d = Vector (x v * d) (y v * d) (z v * d)

reflection :: Vector -> Vector -> Vector
reflection incident normal = 
    let
        dot = dotProduct normal (multiply incident (-1.0))
        v = vector (x incident) (y incident) (z incident)
    in
        normalize (v + (multiply normal (2*dot)))

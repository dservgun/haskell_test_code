module Point where
import Color
class Point a where
    getX :: a -> XAxis
    getY :: a -> YAxis
    move :: a -> a -> a

class (Point a) => ColoredPoint a where
     color :: a -> Color
--Note
-- the axes are defined in terms of the primitive int
-- though the Class Num is defined in terms of Integer allowing for arbitrary precision.
-- fromIntegral converts an Integer to Int.
-- As we can clearly see, perhaps there is a better way to represent inheritance?
-- or is inheritance not worth the trouble supporting.

type XAxis = Int
type YAxis = Int
data TwoDPoint = TwoDPoint XAxis YAxis
    deriving (Show, Ord, Read, Eq)
    
data TwoDColorPoint = TwoDColorPoint TwoDPoint Color

instance Num TwoDPoint where
    (TwoDPoint x1 y1) + (TwoDPoint x2 y2) = TwoDPoint (x1 + x2) (y1 + y2)
    (TwoDPoint x1 y1) - (TwoDPoint x2 y2) = TwoDPoint (x1 - x2)  (y1 - y2)
    abs (TwoDPoint x y) = TwoDPoint (abs x) (abs y)
    (TwoDPoint x1 y1) * (TwoDPoint x2 y2) = TwoDPoint (x1 * x2) (y1 * y2)
    signum x = x
    fromInteger i = TwoDPoint (fromIntegral i) (fromIntegral i)

instance Num TwoDColorPoint where
    (TwoDColorPoint t1 c1) + (TwoDColorPoint t2 c2) = TwoDColorPoint (t1 + t2) (c1 + c2)
    
instance Point TwoDPoint where
    getX (TwoDPoint x _) = x
    getY (TwoDPoint _ y) = y
    move p1 p2 = p1 + p2

instance Point TwoDColorPoint where
    getX (TwoDColorPoint t _) =  getX t
    getY (TwoDColorPoint t _) = getY t
    move p1 p2 = p1 + p2    
instance ColoredPoint TwoDColorPoint where
    color (TwoDColorPoint _ c) = c
        

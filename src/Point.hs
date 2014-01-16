module Point where
import Color
class Point a where
    getX   :: a -> XAxis
    getY   :: a -> YAxis
    moveTo :: a -> a -> a
    moveBy :: a -> a -> a
    distance :: a -> Float

class (Point a) => ColoredPoint a where
     color :: a -> Color
--Note
-- the axes are defined in terms of the primitive int
-- though the Class Num is defined in terms of Integer allowing for arbitrary precision.
-- fromIntegral converts an Integer to Int.
-- As we can clearly see, perhaps there is a better way to represent inheritance?
-- or is inheritance not worth the trouble supporting.
-- Issues: XAxis needs to be either Int or Integer and the points need to 
-- work on both types.
type XAxis = Int
type YAxis = Int

origin = TwoDPoint 0 0

data TwoDPoint = TwoDPoint XAxis YAxis
    deriving (Show, Ord, Read, Eq)
    
data TwoDPointWithColor = TwoDPointWithColor TwoDPoint Color

instance Num TwoDPoint where
    (TwoDPoint x1 y1) + (TwoDPoint x2 y2) = TwoDPoint (x1 + x2) (y1 + y2)
    (TwoDPoint x1 y1) - (TwoDPoint x2 y2) = TwoDPoint (x1 - x2)  (y1 - y2)
    abs (TwoDPoint x y) = TwoDPoint (abs x) (abs y)
    (TwoDPoint x1 y1) * (TwoDPoint x2 y2) = TwoDPoint (x1 * x2) (y1 * y2)
    signum x = x
    fromInteger i = TwoDPoint (fromIntegral i) (fromIntegral i)
    

instance Num TwoDPointWithColor where
    (TwoDPointWithColor t1 c1) + (TwoDPointWithColor t2 c2) = TwoDPointWithColor (t1 + t2) (c1 + c2)
    (TwoDPointWithColor t1 c1) * (TwoDPointWithColor t2 c2) = TwoDPointWithColor (t1 * t2) (c1 * c2)
    
    
instance Point TwoDPoint where
    getX (TwoDPoint x _) = x
    getY (TwoDPoint _ y) = y
    moveTo p1 p2 = p2
    moveBy p1 p2 = p1 + p2
    distance p1 = 
        let 
           hyp (TwoDPoint x y) = x^2 + y^2 in
            sqrt $ fromIntegral $ hyp(p1 - origin)
 

instance Point TwoDPointWithColor where
    getX (TwoDPointWithColor t _) =  getX t
    getY (TwoDPointWithColor t _) = getY t
    moveTo p1 p2 = p2    
    moveBy p1 p2 = p1 + p2
instance ColoredPoint TwoDPointWithColor where
    color (TwoDPointWithColor _ c) = c
        

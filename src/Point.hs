module Point where

type Red = Int
type Blue = Int
type Green = Int
type Cyan = Int
type Magenta = Int
type Yellow = Int
type Key = Int -- Key and black are synonyms
type Black = Int

data Color = RGB (Red, Blue, Green)  | CMYK (Cyan, Magenta, Yellow, Key)
-- This file attempts to implement the point hierarchy as defined in 
-- ocaml manual.
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
type XAxis = Int
type YAxis = Int
data TwoDPoint = TwoDPoint XAxis YAxis
    deriving (Show, Ord, Read, Eq)
instance Num TwoDPoint where
    (TwoDPoint x1 y1) + (TwoDPoint x2 y2) = TwoDPoint (x1 + x2) (y1 + y2)
    (TwoDPoint x1 y1) - (TwoDPoint x2 y2) = TwoDPoint (x1 - x2)  (y1 - y2)
    abs (TwoDPoint x y) = TwoDPoint (abs x) (abs y)
    (TwoDPoint x1 y1) * (TwoDPoint x2 y2) = TwoDPoint (x1 * x2) (y1 * y2)
    signum x = x
    fromInteger i = TwoDPoint (fromIntegral i) (fromIntegral i)
     
instance Point TwoDPoint where
    getX (TwoDPoint x _) = x
    getY (TwoDPoint _ y) = y
    move p1 p2 = p1 + p2
    
        

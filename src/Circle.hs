module Circle where

import Point(TwoDPoint, TwoDPointWithColor, moveTo, moveBy)

type Center = TwoDPoint
type CenterWithColor = TwoDPointWithColor
-- In the original example in ocaml, the circle was modeled 
-- an object-oriented class. Initial attempt to model it as a class
-- may be overkill and we can achieve the functionality with a couple of 
-- functions.
data Circle = Circle Center
data CircleWithColor = CircleWithColor CenterWithColor

moveTo,moveBy :: Circle -> Center ->Circle
moveTo o@(Circle aPoint) newPoint = Circle newPoint
moveBy o@(Circle aPoint) newPoint = Circle (Point.moveBy aPoint newPoint)


module Money where

-- How to model a value?    
class (Num a, Eq a, Ord a) => Money a where
    convert :: a -> a
    normalize :: a -> Rational
       

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Arrows #-}
-- This template is documented more completely in the tutorial at
-- https://www.fpcomplete.com/school/advanced-haskell-1/xml-parsing-with-validation

module Main where
import State
import Supply

main = rand >>= \x -> putStr (show x)

{-# LANGUAGE NoMonomorphismRestriction #-}
-- This template is documented more completely in the tutorial at
-- https://www.fpcomplete.com/school/advanced-haskell-1/xml-parsing-with-validation

module Main where
import State
import Supply
import System.Random
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import HandleIO
import TransactionQb
import CountEntriesT
import TestHttpConduit
import System.Environment
testTransactions = createTransaction salesRefund  dateFromToday dummyNumber prototype dummyAmount

-- The Reader/IO combined monad, where Reader stores a string.
printReaderContent = do
    content <- ask
    liftIO $ getEnv $ "APPROOT"

divides d n = rem n d == 0
ld n = ldf 2 n
ldf k n | divides k n = k
    | k ^2 > n = n
    | otherwise = ldf (k + 1) n
    
charCount :: Char -> [Char] -> Int
charCount a [] = 0
charCount a (x:xs) = 
                    if a == x then 
                        1 + (charCount a xs)
                    else
                        charCount a xs

myMap :: (a -> b) ->  [a] -> [b]
myMap f [] = []
myMap f (x:s) = (f x) : (myMap f s)

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x: xs)
        | a <= x  = a : x : xs
        | otherwise = x : (insert a xs)
insSort :: Ord a => [a] -> [a]        
insSort [] = []
insSort (x : xs) = insert x (insSort xs)

data STree a = Leaf | Node a (STree a) (STree a)
insTree :: Ord a => a -> STree a -> STree a
insTree a Leaf = Node a Leaf Leaf
insTree a (Node x l r)  
        | a <= x = Node x (insTree a l) r
        | otherwise = Node x l (insTree a r)


main =  print "hello"




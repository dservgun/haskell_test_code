{-# LANGUAGE NoMonomorphismRestriction #-}
-- This template is documented more completely in the tutorial at
-- https://www.fpcomplete.com/school/advanced-haskell-1/xml-parsing-with-validation

module Main where
import State
import Supply
import System.Random
import Control.Monad
import Control.Monad.State
import HandleIO
import TransactionQb

testTransactions = createTransaction salesRefund  dateFromToday dummyNumber prototype dummyAmount
main = runHandleIO (safeHello "goodbye")

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply
    (Supply
    , next
    , runSupply, getSupply) where

import Control.Monad.State


newtype Supply s a = S (State [s] a)
    deriving(Monad)

next = S $ do st <- get
              case st of 
                  [] -> return Nothing
                  (x:xs) -> do put xs
                               return (Just x)
                            
runSupply (S m) xs = runState m xs

getSupply (S m) xs = return (runSupply (S m) xs)


{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Arrows #-}
-- This template is documented more completely in the tutorial at
-- https://www.fpcomplete.com/school/advanced-haskell-1/xml-parsing-with-validation

module Main where

import Text.XML.HXT.Core
import Text.XML.HXT.RelaxNG
import Data.Fixed (Centi)
import Data.Text (Text, pack)
import Control.Monad(forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Test.QuickCheck
import FYComputations



type Comment = Maybe Text
data Transaction = Deposit !Text !Centi !Comment
                 | Withdrawal !Text !Centi !Comment
                 deriving Show

make "deposit" = Deposit
make "withdrawal" = Withdrawal
make trans = error $ "Invalid transaction type: " ++ trans

-- The select function should select the elements that are going to be processed.
select = getChildren >>>  hasName "transactions" >>> getChildren >>> isElem

-- The transform function transforms each element into a Haskell Data structure.
transform = proc el -> do
  trans <- getName -< el
  account <- getAttrValue "account" -< el
  amount <- getAttrValue "amount" -< el
  comment <- withDefault (Just . pack ^<< getText <<< getChildren) Nothing -< el
  returnA -< (make trans) (pack account) (read amount) comment

-- The process function handles each Haskell data object in the XML file
process = arrIO print

-- The handleError function is called if there is an error in the input XML.
handleError = getAttrValue a_source >>> arrIO (putStrLn . ("Error in document: " ++))

main :: IO ()
mainXML = do
  runX $ readDocument [withRelaxNG "transactions.rng"] "transactions.xml"
         >>> ((select >>> transform >>> process) `orElse` handleError)
  return ()

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
                let path = topdir </> name
                isDirectory <- doesDirectoryExist path
                if isDirectory
                    then getRecursiveContents path
                else
                    return [path]
    return (concat paths)
    
    
filepathToAction f = putStrLn (show f)
list2Actions  = map filepathToAction 
runall [] = return ()
runall (h:tail) = h >> (runall tail)

main = putStrLn "Hello world"
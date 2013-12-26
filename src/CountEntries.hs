module CountEntries where

import System.Directory(doesDirectoryExist, getDirectoryContents)
import System.FilePath((</>))
import Control.Monad (forM, liftM)

notDots :: FilePath -> Bool
notDots = \s -> s /="." && s /= ".."

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents

listDirectoryAlt :: FilePath -> IO [String]
listDirectoryAlt = \m -> let d = getDirectoryContents m in
                             d >>= \s -> return (filter notDots s)

listDirectoryAlt2 :: FilePath -> IO [String]                             
listDirectoryAlt2  = \fp -> (filter notDots) `liftM` (getDirectoryContents fp)


                      
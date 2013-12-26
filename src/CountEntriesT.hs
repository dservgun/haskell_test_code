module CountEntriesT where
--Design note: of course this is from the RWH book and 
--the design choices simply stand out or is it that
--haskell lends elegance once we start to look for it?
--anyway, here is the code, since this is a transformer,
--this is not part of the core count entries module.

import CountEntries(listDirectory)
import System.Directory(doesDirectoryExist)
import System.FilePath((</>))
import Control.Monad(forM_, when)
import Control.Monad.Trans(liftIO)
import Control.Monad.Writer (WriterT, tell)

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
     contents <- liftIO (listDirectory path)
     tell [(path, length contents)]
     forM_ contents $ \name -> do
         let newName = path </> name
         isDir <- liftIO (doesDirectoryExist newName)
         when isDir (countEntries newName)
         
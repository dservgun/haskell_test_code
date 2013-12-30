module StackingMonads where
import System.Directory
import System.FilePath
import Control.Monad.Reader
import Control.Monad.State
import CountEntries

data AppConfig = AppConfig {
    cfgMaxDepth :: Int
} deriving (Show)

data AppState = AppState {
    stDeepestReached :: Int
}deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)
    
runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth = 
    let config = AppConfig maxDepth
        state = AppState 0
        in runStateT (runReaderT k config) state

constrainedCount curDepth path = do
    contents <- liftIO . listDirectory $ path
    cfg <- ask
    let maxDepth = cfgMaxDepth cfg
    rest <- forM contents $ \name -> do
        let newPath = path </> name
        isDir <- liftIO $ doesDirectoryExist newPath
        if isDir && curDepth < maxDepth
            then do
                let newDepth = curDepth + 1
                st <- get
                when (stDeepestReached st < newDepth) $
                        put st {stDeepestReached = newDepth}
                constrainedCount newDepth newPath
        else return []
    return $ (path, length contents) : concat rest
            
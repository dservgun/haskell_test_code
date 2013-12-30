{-# LANGUAGE OverloadedStrings #-}
module TestHttpConduit
    (runGoogle)
where
import Network.HTTP.Conduit
import Data.Conduit
import Data.Conduit.Binary(sinkFile)
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)

-- Process the result byte string by saving the transaction in 
-- the database. 

parse aResponse = responseBody aResponse
runGoogle :: IO ()
runGoogle = do
     -- We are in the IO monad
     runResourceT $ do
         -- Now we are in the resource transformer monad.
         -- the idea being that the exception path
         -- is handled inside the ResourceTransformer monad
         manager <- liftIO $ newManager def
         -- the newManager call returns an IO Manager so liftIO 
         -- is a generic operation that lifts the manager from IO
         -- and puts it in the resource transformer.
         --
         req <- liftIO $ parseUrl "http://www.google.com"
         res <- httpLbs req manager 
         -- Process this response
         -- save it in the database, break out the components and do something like that.
         let resultByteString =  parse res in             
             liftIO $ L.putStr $ resultByteString
         
         
         

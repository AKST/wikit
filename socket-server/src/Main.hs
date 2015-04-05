{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (log)

import qualified System.Log.Logger as L

import Data.API
import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T

import Control.Monad (void)
import Control.Monad.Except
import Control.Applicative

import Network.Connection
import Network.Wikipedia

import Common



main :: IO ()
main = initWS "0.0.0.0" 8080 $
  decode <$> awaitData >>= \case 
    Nothing -> do
      log L.ERROR "could not parse request" 
      closeConnection CouldntParseRequest
    Just (Request request timestamp) -> dispatchVoid $ do 
      --
      -- The handle forks the thread to allow the 
      -- connection to accept additional jobs
      --
      catchError 

        (do
          response <- withRequest request
          yieldResponse (Response response timestamp)) 

        (\error -> do 
          yieldResponse (ErrorRes error timestamp))

      
withRequest request = case request of

  -- checks if article exists
  WCheck name -> do
    (ExistsRes exists) <- articleExists name 
    return (WExists exists)

  -- get the first revisions of an article 
  WStart name -> do
    (RevisionRes revisions) <- getRevisions 
      (revisionQuery { article = name }) 
    return (WRevisions revisions)
    
  -- get the rest of the article revisions
  WCont name cont -> do
    (RevisionRes revisions) <- getRevisions 
      (revisionQuery { article = name, rvcontinue = Just cont }) 
    return (WRevisions revisions)




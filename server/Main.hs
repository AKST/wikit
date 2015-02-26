{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T

import Control.Monad (void)
import Control.Applicative

import Connection
import Wikipedia
import API


main :: IO ()
main = initWS "0.0.0.0" 8080 $
  decode <$> awaitData >>= \case 
    Nothing      -> yieldResponse WParseError
    Just request -> dispatchVoid $ do 
      --
      -- The handle forks the thread to allow the 
      -- connection to accept additional jobs
      --
      case request of

        -- get the first revisions of an article 
        WStart name -> do
          revisions <- getRevisions (wikiQuery { article = name }) 
          yieldResponse (WRevisions revisions)

        -- get the rest of the article revisions
        WCont name cont -> do
          revisions <- getRevisions (wikiQuery { 
            article    = name, 
            rvcontinue = Just cont }) 
          yieldResponse (WRevisions revisions)



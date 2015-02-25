{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (log)

import qualified Network.WebSockets as WS

import System.Log (Priority(INFO))

import Data.Word
import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid (mappend)
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Control.Monad (forever, void)
import Control.Applicative

import Connection
import Wikipedia
import API


handle :: WikiTReq -> Conn ()
handle request = void . dispatch $ do 
  --
  -- The handle forks the thread to allow the 
  -- connection to accept additional jobs
  --
  case request of
    WStart name -> do
      log INFO ("fetching the revisions for \"" ++ T.unpack name ++ "\"")
      revisions <- getRevisions (wikiQuery { article = name }) 
      yieldResponse (WRevisions revisions)
    WCont name cont -> do
      log INFO ("continuing the revisions for \"" ++ T.unpack name ++ "\"")
      revisions <- getRevisions (wikiQuery { 
        article    = name, 
        rvcontinue = Just cont }) 
      yieldResponse (WRevisions revisions)


main :: IO ()
main = WS.runServer "0.0.0.0" 8080 $ \pending -> do
  connection <- WS.acceptRequest pending
  WS.forkPingThread connection 30
  --
  -- This runs forever, and wil decode incoming requests,
  -- if unsuccessful it will just return a parse error to
  -- the client, otherwise it will handle the request as normal
  -- 
  runConnection (ConnOpts connection) $
    decode <$> awaitData >>= \case 
      Nothing          -> yieldResponse WParseError
      Just instruction -> handle instruction



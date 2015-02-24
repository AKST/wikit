{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


import qualified Network.WebSockets as WS

import Data.Word
import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid (mappend)

import qualified Data.Text          as T
import qualified Data.Text.IO       as T

import Control.Monad (forever, void)

import Connection
import Wikipedia
import API


handle :: WikiTReq -> Conn ()
handle request = void . dispatch $ do 
  yieldResponse (WEcho request) 


main :: IO ()
main = WS.runServer "0.0.0.0" 8080 $ \pending -> do
  connection <- WS.acceptRequest pending
  WS.forkPingThread connection 30
  runConnection (ConnOpts connection) $
    fmap decode awaitData >>= \case 
      Nothing          -> yieldResponse WParseError
      Just instruction -> handle instruction



{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


import qualified Network.WebSockets as WS

import Data.Word
import Data.Aeson
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid (mappend)

import qualified Data.Text          as T
import qualified Data.Text.IO       as T

import Control.Monad (forever)

import API


main :: IO ()
main = WS.runServer "0.0.0.0" 8080 app where 

  app pending = do
    connection <- WS.acceptRequest pending
    WS.forkPingThread connection 30
    forever $ do
      request  <- WS.receiveData connection 
      response <- forRequest request 
      WS.sendTextData connection response

  forRequest = handle onParse WParseError where
    onParse request = return (WEcho request) 


handle :: (Monad m, FromJSON a, ToJSON b, ToJSON c) 
       => (a -> m b) 
       -> c 
       -> ByteString 
       -> m ByteString

handle onParse onFail message = 
  let parsed = decode message in case parsed of
    (Just instruction) -> do
      result <- onParse instruction
      return (encode result) 
    Nothing -> 
      return (encode onFail)



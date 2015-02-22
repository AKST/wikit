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

import API


main :: IO ()
main = WS.runServer "0.0.0.0" 8080 app where 

  app pending = do
    connection <- WS.acceptRequest pending
    request    <- WS.receiveData connection 
    WS.sendTextData connection (forRequest request) 

  forRequest = handle onParse WParseError where
    onParse = WEcho 


handle :: (FromJSON a, ToJSON b, ToJSON c) => (a -> b) -> c -> ByteString -> ByteString
handle onParse onFail message = 
  let parsed = decode message in case parsed of
    (Just instruction) -> encode $ onParse instruction  
    Nothing            -> encode $ onFail



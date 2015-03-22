module Network.MessageStore (

  initStore,
  send,
  MessageStore(..),
  FatalError(..)
  
) where

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Core (Json(..))
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson, decodeMaybe)
import Data.Maybe (Maybe(..))

import Network.WebSocket (WebSocket(..), SocketError(..))

import Control.Monad.Eff
import Control.Monad.Eff.Class


{------------------------------------------------------
  Types
------------------------------------------------------}


foreign import data MessageStore :: *

newtype FatalError = FatalError { 
  status :: String, 
  body :: { message :: String }
}

newtype StoreResponse = StoreResponse Json

instance showFatalError :: Show FatalError where
  show (FatalError obj) = implShow obj

foreign import implShow """
  function implShow(obj) {
    if (typeof obj.toString !== 'undefined') {
      return obj.toString();
    }
    else {
      return JSON.stringify(obj);
    }
  }""" :: forall a. a -> String

{------------------------------------------------------
  Implementation
------------------------------------------------------}


foreign import initStore """
  function initStore(url) {
    return function (protocols) {
      return function (onFatalError) {
        return function () {
          return window.MessageStore.create(
            url, 
            protocols, 
            function (e) {
              onFatalError(e)();
            }
          );
        };
      };
    };
  }""" :: forall e e'. String 
       -> [String]
       -> (FatalError -> Eff e Unit)
       -> Eff (ws :: WebSocket | e') MessageStore


foreign import sendImpl """
  function sendImpl(store) {
    return function (callback) {
      return function (request) {
        return function () {
          store.send(request, function (response) {
            callback(response)(); 
          });
        };
      };
    };
  }
  """ :: forall e e'. MessageStore 
      -> (StoreResponse -> Eff e Unit) 
      -> Json 
      -> Eff (ws :: WebSocket | e') Unit


send :: forall a b e e'. (DecodeJson a, EncodeJson b) 
     => MessageStore -> (a -> Eff e Unit) -> b -> Eff (ws :: WebSocket | e') Unit 

--
-- Todo make function handle an either
--
send store callback request = sendImpl store onResponse serialised where

  serialised = encodeJson request
  
  onResponse :: StoreResponse -> Eff e Unit
  onResponse (StoreResponse resp) = case decodeMaybe resp of
    Just response -> callback response
    Nothing       -> return unit



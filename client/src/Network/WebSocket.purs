module Network.WebSocket (
  open, 
  send, 
  WebSocket(..), 
  Socket(..), 
  SocketError(..), 
  MessageEvent(..), 

  onMessage,
  onError,
  onClose

) where

import Control.Monad.Eff
import Control.Monad.Eff.Class

foreign import data WebSocket :: !
foreign import data Socket :: *

newtype SocketError = SocketError {}
newtype MessageEvent = MessageEvent { data :: String } 


{------------------------------------------------------
Typeclass instances
------------------------------------------------------}

foreign import showObj """
  function showObj(messageEvent) { 
    return messageEvent.toString(); 
  }
  """ :: forall a. a -> String

instance messageEvent :: Show MessageEvent where
  show (MessageEvent evt) = showObj evt

instance socketErrorShow :: Show SocketError where
  show (SocketError error) = showObj error


{------------------------------------------------------
  WebSocket + Socket
------------------------------------------------------}


--
-- Note that the socket is wrapped in a promise, this
-- is just to get rid of the whole onopen callback.
--
--     var socket = new WebSocket(url);
--     socket.onopen = function () { ... };
--
foreign import open """
  function open(url) {
    return function (protocols) {
      return function () {
        var socket;
        if (protocols.length) {
          socket = new WebSocket(url);
        }
        else {
          socket = new WebSocket(url, protocols);
        }
        return new window.RSVP.Promise(function (resolve) {
          socket.onopen = function (event) {
            resolve(socket);
          };
        });
      };
    };
  } """ :: forall e. String -> [String] -> Eff (ws :: WebSocket | e) Socket


foreign import onEvent """ 
  function onEvent(eventName) {
    return function (socket) {
      return function (callback) {
        return function () {
          socket.then(function (socket) {
            socket.addEventListener(eventName, function (evt) {
              callback(evt)();
            });
          });
        };
      };
    };
  } """ :: forall a e e'. String -> Socket -> (a -> Eff e Unit) -> Eff (ws :: WebSocket | e') Unit


onMessage :: forall e e'. Socket -> (MessageEvent -> Eff e Unit) -> Eff (ws :: WebSocket | e') Unit
onMessage = onEvent "message"


onError :: forall e e'. Socket -> (SocketError -> Eff e Unit) -> Eff (ws :: WebSocket | e') Unit
onError = onEvent "error"


onClose :: forall e e'. Socket -> Eff e Unit -> Eff (ws :: WebSocket | e') Unit
onClose socket event = onEvent "close" socket \_ -> event


foreign import send """
  function send(socket) {
    return function (data) {
      return function () {
        socket.then(function (socket) {
          socket.send(data);
        });
      };
    };
  }
  """ :: forall e. Socket -> String -> Eff (ws :: WebSocket | e) Unit


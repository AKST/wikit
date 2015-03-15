module Network.WebSocket (
  open, 
  send, 
  WebSocket(..), 
  Socket(..), 
  SocketError(..), 
  onMessage,
  onError,
  onClose
) where

import Control.Monad.Eff
import Control.Monad.Eff.Class

foreign import data WebSocket :: !
foreign import data Socket :: *
foreign import data SocketError :: *


{------------------------------------------------------
  SocketError
------------------------------------------------------}


instance socketErrorShow :: Show SocketError where
  show = showError

foreign import showError """
  function showError(error) { 
    return error.toString(); 
  }
  """ :: SocketError -> String


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
    return function () {
      var socket = new WebSocket(url);
      return new window.RSVP.Promise(function (resolve) {
        socket.addEventListener("open", function (event) {
          resolve(socket);
        });
      });
    };
  } """ :: forall e. String -> Eff (ws :: WebSocket | e) Socket


foreign import onEvent """ 
  function onEvent(eventName) {
    return function (socket) {
      return function (callback) {
        return function () {
          socket.then(function (socket) {
            socket.addEventListener(eventName, callback);
          });
        };
      };
    };
  } """ :: forall a e e'. String -> Socket -> (a -> Eff e Unit) -> Eff (ws :: WebSocket | e') Unit


onMessage :: forall e e'. Socket -> (String -> Eff e Unit) -> Eff (ws :: WebSocket | e') Unit
onMessage = onEvent "message"


onError :: forall e e'. Socket -> (SocketError -> Eff e Unit) -> Eff (ws :: WebSocket | e') Unit
onError = onEvent "error"


onClose :: forall e e'. Socket -> (Unit -> Eff e Unit) -> Eff (ws :: WebSocket | e') Unit
onClose = onEvent "close"


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


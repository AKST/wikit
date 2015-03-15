module Network.WebSocketAPI where


import Data.Maybe


type WikiR = { name :: String, continue :: Maybe Number } 


foreign import serialise """
  function serialise(request) {
    if (request.continue === null) {
      return JSON.stringify({ name: request.name });
    }
    else {
      return JSON.stringify(request);
    }
  }
  """ :: WikiR -> String


module Network.WebSocketAPI where


import Data.Maybe


type WikiRequest  = { name :: String, continue :: Maybe Number } 
type WikiResponse = { status :: String } 


foreign import serialise """
  function serialise(request) {
    if (request.continue instanceof PS.Data_Maybe.Just) {
      return JSON.stringify({ 
        name: request.name,
        continue: request.continue.value0 
      });
    }
    else {
      return JSON.stringify({ 
        name: request.name
      });
    }
  }
  """ :: WikiRequest -> String


foreign import deserialise """
  function deserialise(response) {
    return JSON.parse(response);
  }
  """ :: String -> WikiResponse




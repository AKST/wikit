module Main where
  
import Data.Either (Either (Left, Right))
import Control.Monad.Trans (lift)
import Debug.Trace (trace)

import HTML5.WebSocket

main = do 
  trace "Hello, world!"
  runWebSocket $ do
    result <- withWebSocket config handlers
    case result of
      Left err -> (lift <<< trace) err
      Right _ -> (lift <<< trace) "DONE" -- rest

  where 

    config = { uri: "ws://0.0.0.0:8080", protocols: ["ws"] }

    handlers s = (defaultHandlers s) {
      onOpen    = trace "OPEN",
      onMessage = \m -> trace m
    }


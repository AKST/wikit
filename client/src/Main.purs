module Main where

import Data.Maybe

import Debug.Trace (trace, Trace(..))

import Network.Routing.Client
import qualified Network.WebSocket as WS

import Control.Monad.Eff
import Control.Monad.Eff.Class
import qualified Control.Monad.JQuery as J

import qualified Thermite as T

import Components.Article 
import Components.Query 

import DOM


main = do
  socket <- WS.open "ws://0.0.0.0:8080" []
  socket `WS.onMessage` messageListener 
  socket `WS.onError` errorListener
  socket `WS.onClose` closeListener

  runRouter (routerConfig socket) where

    messageListener message = do
      trace (show message)

    errorListener error = do
      trace (show error)

    closeListener = do
      trace "socket has been closed"

    routerConfig socket = do
      let queryClass   = T.createClass queryPage
          articleClass = T.createClass articlePage

      articleName <- param any
      arciclePreF <- param (exact "article")

      route0 empty do
        liftEff (trace "hello world")
        onReady queryClass { 
          socket: socket, 
          message: Nothing 
        }

      route1 (arciclePreF -/ articleName +/ empty) $ \name -> do
        void $ liftEff do
          trace ("now viewing " ++ name)
          onReady articleClass {}

      notFound do
        liftEff (trace "on found, redirecting to index")
        setRoute "/"


onReady component props = J.ready (T.render component props)



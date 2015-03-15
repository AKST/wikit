module Main where


import Debug.Trace (trace, Trace(..))

import Network.Routing.Client
import qualified Network.WebSocket as WS

import Control.Monad.Eff
import Control.Monad.Eff.Class
import qualified Control.Monad.JQuery as J

import qualified Thermite as T

import Components.App

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
      let mainApp = T.createClass app
          mainPps = { socket: socket }

      articleName <- param any
      arciclePreF <- param (exact "article")

      route0 empty do
        liftEff (trace "hello world")
        onReady mainApp mainPps

      route1 (arciclePreF -/ articleName +/ empty) $ \name -> do
        void $ liftEff do
          trace ("now viewing " ++ name)
          onReady mainApp mainPps

      notFound do
        liftEff (trace "on found, redirecting to index")
        setRoute "/"


onReady component props = J.ready (T.render component props)



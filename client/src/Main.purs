module Main where


import Debug.Trace (trace)

import Network.Routing.Client

import Control.Monad.Eff
import Control.Monad.Eff.Class
import qualified Control.Monad.JQuery as J

import qualified Thermite as T

import Components.App

import DOM


main = do
  let mainApp = T.createClass app
      
  runRouter do

    articleName <- param any
    arciclePreF <- param (exact "article")

    route0 empty do
      liftEff (trace "hello world")
      onReady mainApp {}

    route1 (arciclePreF -/ articleName +/ empty) $ \name -> do
      void $ liftEff do
        trace ("now viewing " ++ name)
        onReady mainApp {}

    notFound do
      liftEff (trace "on found, redirecting to index")
      setRoute "/"


onReady component props = J.ready (T.render component props)



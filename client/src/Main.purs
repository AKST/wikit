module Main where

import Data.Maybe

import Debug.Trace (trace, Trace(..))

import Network.Routing.Client
import qualified Network.MessageStore as MS

import Control.Monad.Eff
import Control.Monad.Eff.Class
import qualified Control.Monad.JQuery as J

import qualified Thermite as T

import Components.Article 
import Components.Query 

import DOM


main = do
  store <- MS.initStore "ws://0.0.0.0:8080" [] onCrash
  runRouter (routerConfig store) where

    onCrash message = do
      trace ("store has crashed " ++ show message)

    routerConfig store = do
      let queryClass   = T.createClass queryPage
          articleClass = T.createClass articlePage

      articleName <- param any
      arciclePreF <- param (exact "article")

      route0 empty do
        liftEff (trace "hello world")
        onReady queryClass { 
          store: store, 
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



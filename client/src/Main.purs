module Main where

import Debug.Trace (trace, Trace(..))

import Control.Monad.Eff
import qualified Control.Monad.JQuery as J

import DOM


main = do 
  trace "Welcome to Wikit"
  J.ready do
    heading  <- J.create "<h1>" >>= J.setText "Hello, world!"
    pageBody <- J.select "#app"
    J.addClass "container" pageBody
    heading `J.append` pageBody




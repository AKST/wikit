module Main where


import Debug.Trace (trace, Trace(..))

import Control.Monad.Eff
import qualified Control.Monad.JQuery as J

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T


main = J.ready do
  T.render app {}


app = T.createClass (T.simpleSpec {} performAction render) where

  performAction _ _ = do
    T.modifyState \_ -> {}

  render ctx _ _ = T.div [A._id "app", A.className "container"] [
    T.h1' [T.text "Hello World"],
    T.p'  [T.text "Welcome to my home page (-:"]
  ]


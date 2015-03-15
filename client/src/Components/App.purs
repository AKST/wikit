module Components.App (app, AppPs(), AppSt(), AppEv(..), AppEf(..)) where

import Data.Maybe

import Debug.Trace (trace, Trace(..))

import Control.Monad.Eff
import Control.Monad.Eff.Class

import qualified Network.Routing.Client as R
import qualified Network.WebSocket as WS
import qualified Network.WebSocketAPI as WS

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

import Components.Common


data AppEv = AppDoNothing | AppSearch String
type AppPs = { socket :: WS.Socket }
type AppSt = {}
type AppEf e = (trace :: Trace, routing :: R.Routing, ws :: WS.WebSocket | e)


app :: forall e. T.Spec (T.Action (AppEf e) AppSt) AppSt AppPs AppEv
app = T.simpleSpec {} performAction render where

  performAction :: T.PerformAction AppPs AppEv (T.Action (AppEf e) AppSt)
  performAction _ AppDoNothing = return unit 
  performAction { socket: ws } (AppSearch articleName) = T.sync do
      trace ("requesting revisions for " ++ articleName)
      ws `WS.send` WS.serialise { name: articleName, continue: Nothing } 


  handleInput :: T.KeyboardEvent -> AppEv
  handleInput keyevent = case currentK of
    "Enter" -> AppSearch (getValue keyevent)
    _       -> AppDoNothing
    where currentK = keyCode keyevent
          currentT = getValue keyevent

  render :: T.Render AppSt AppPs AppEv
  render ctx _ _ = T.div [A._id "app", A.className "container"] [
    T.h1' [T.text "Hello World"],
    T.p'  [T.text "Welcome to my home page (-:"],
    T.input [T.onKeyPress ctx handleInput] []
  ]



module Components.App where

import Debug.Trace (trace, Trace(..))

import Control.Monad.Eff
import Control.Monad.Eff.Class

import qualified Network.Routing.Client as R

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T


data AppEv
  = AppDoNothing
  | AppSearch String


type AppPs = {}
type AppSt = {}

app :: forall e. T.Spec _ AppSt _ _
app = T.simpleSpec {} performAction render where

  performAction :: T.PerformAction AppPs AppEv _
  performAction _ e = case e of 
    AppDoNothing -> do
      T.modifyState \_ -> {}
      return unit
    AppSearch ws -> do
      return unit

  handleInput keyevent = case currentK of
    "Enter" -> AppSearch (getValue keyevent)
    _       -> AppDoNothing
    where currentK = keyCode keyevent
          currentT = getValue keyevent

  render ctx _ _ = T.div [A._id "app", A.className "container"] [
    T.h1' [T.text "Hello World"],
    T.p'  [T.text "Welcome to my home page (-:"],
    T.input [T.onKeyPress ctx handleInput] []
  ]


foreign import getValue """
  function getValue(e) {
    return e.target.value;
  }
  """ :: T.KeyboardEvent -> String

foreign import keyCode """
  function keyCode(e) {
    return e.key;
  }
  """ :: T.KeyboardEvent -> String


module Components.Query (
  
  queryPage, 
  QueryPs(), 
  QuerySt(), 
  QueryEv(), 
  QueryEf(..)

) where

import Data.Argonaut (printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Maybe
import Data.Date

import Debug.Trace (trace, Trace(..))

import Control.Monad.Eff
import Control.Monad.Eff.Class

import qualified Network.Routing.Client as R
import qualified Network.WebSocket    as WS
import qualified Network.WebSocketAPI as WS
import qualified Network.MessageStore as MS

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

import Components.Common

import Model.Message
import Model.Status


data QueryEv = DoNothing | Search String
type QueryPs = { store :: MS.MessageStore, message :: Maybe Message }
type QuerySt = {}
type QueryEf e = (trace :: Trace, routing :: R.Routing, ws :: WS.WebSocket | e)


--
-- TODO rename as query
--
queryPage :: forall e. T.Spec (T.Action (QueryEf e) QuerySt) QuerySt QueryPs QueryEv
queryPage = T.simpleSpec {} performAction render where

  performAction :: T.PerformAction QueryPs QueryEv (T.Action (QueryEf e) QuerySt)
  performAction _ DoNothing = return unit 
  performAction { store: ms } (Search articleName) = T.sync do
    trace ("checking the availablity of articles for " ++ articleName)
    MS.send ms onResponse (WS.WikiRequest { 
      name: articleName, 
      continue: Nothing
    }) 

  onResponse (WS.WikiResponse { status: s }) =
    trace ("response was " ++ s) 

  handleInput :: T.KeyboardEvent -> QueryEv
  handleInput keyevent = case currentK of
    "Enter" -> Search (getValue keyevent)
    _       -> DoNothing
    where currentK = keyCode keyevent
          currentT = getValue keyevent

  render :: T.Render QuerySt QueryPs QueryEv
  render ctx _ _ = T.div [A._id "app", A.className "container"] [
    T.h1' [T.text "Hello World"],
    T.p'  [T.text "Welcome to my home page (-:"],
    T.input [T.onKeyPress ctx handleInput] []
  ]



module Components.Query (
  
  queryPage, 
  QuerySt(), 
  QueryEv(), 
  QueryPs(..), 
  QueryEf(..)

) where

import Data.Argonaut (printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Maybe
import Data.Date
import Data.Message
import Data.Status
import Data.API

import Debug.Trace (trace, Trace(..))

import Control.Monad.Eff
import Control.Monad.Eff.Class

import qualified Network.Routing.Client as R
import qualified Network.WebSocket    as WS
import qualified Network.MessageStore as MS

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

import Components.Common
import Components.Typography


data QueryEv = DoNothing | Search String
type QuerySt = { message :: Maybe String }
type QueryPs = { store :: MS.MessageStore, setRoute :: forall e. R.SetRoute e }
type QueryEf e = (trace :: Trace, routing :: R.Routing, ws :: WS.WebSocket | e)


--queryPage :: forall e. T.Spec (T.Action (QueryEf e) QuerySt) QuerySt QueryPs QueryEv
queryPage = T.simpleSpec initialState performAction render where


  initialState = { message: Nothing }


  --performAction :: T.PerformAction QueryPs QueryEv (T.Action (QueryEf e) QuerySt)
  performAction _ DoNothing = return unit 
  performAction props (Search articleName) = T.sync do
    trace ("checking the availablity of articles for " ++ show articleName)
    MS.send props.store (onResponse props.setRoute) (QArticleExists articleName)


  onResponse :: _ -> WikiResponse -> Eff _ Unit
  onResponse set respsonse = case respsonse of
    WikiResponseR result -> case result of 
      AArticleExist name exists ->
        if exists then do
          trace (show name ++ " exists, redirecting") 
          set ("article/" ++ name)
        else
          trace (show name ++ " does not exist") 
      _ ->
        trace "for some reason the response was not an exists"
    WikiResponseE error -> case error of
      InternalError message ->
        trace ("request failed because " ++ show message)


  --handleInput :: T.KeyboardEvent -> QueryEv
  handleInput keyevent = case currentK of
    "Enter" -> Search (getValue keyevent)
    _       -> DoNothing
    where currentK = keyCode keyevent
          currentT = getValue keyevent


  --render :: T.Render QuerySt QueryPs QueryEv
  render ctx _ _ = T.div [A._id "app", A.className "container"] [
    header "WikiT" "app-heading",
    T.input [T.onKeyPress ctx handleInput] []
  ]



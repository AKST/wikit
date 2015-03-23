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

import Model.Message
import Model.Status

import Model.API


data QueryEv = DoNothing | Search String
type QuerySt = { message :: Maybe String }
type QueryPs e = { store :: MS.MessageStore, setRoute :: R.SetRoute e }
type QueryEf e = (trace :: Trace, routing :: R.Routing, ws :: WS.WebSocket | e)


--
-- TODO rename as query
--
queryPage = T.simpleSpec initialState performAction render where


  initialState = { message: Nothing }


  performAction _ DoNothing = return unit 
  performAction props (Search articleName) = T.sync do
    trace ("checking the availablity of articles for " ++ show articleName)
    MS.send props.store 
      (onResponse props.setRoute) 
      (QArticleExists { name: articleName })


  onResponse set respsonse = case respsonse of
    WikiResponseR result -> case result of 
      AArticleExist { name: n, exists: e } ->
        if e then do
          trace (show n ++ " exists, redirecting") 
          set ("article/" ++ n)
        else
          trace (show n ++ " does not exist") 
      _ ->
        trace "for some reason the response was not an exists"
    WikiResponseE error -> case error of
      InternalError message ->
        trace ("request failed because " ++ show message)


  handleInput keyevent = case currentK of
    "Enter" -> Search (getValue keyevent)
    _       -> DoNothing
    where currentK = keyCode keyevent
          currentT = getValue keyevent


  render ctx _ _ = T.div [A._id "app", A.className "container"] [
    T.h1 [A.className "app-heading"] [T.text "WikiT"],
    T.input [T.onKeyPress ctx handleInput] []
  ]



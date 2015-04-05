module Components.Article where

import Control.Monad.Eff
import Control.Monad.Eff.Class

import Debug.Trace (trace, Trace(..))

import qualified Network.Routing.Client as R
import qualified Network.WebSocket as WS
import qualified Network.MessageStore as MS

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

import Components.Common

import qualified Data.Array as Array

import Model.Revision
import Model.API


type ArticlePs = { store :: MS.MessageStore, article :: String }
type ArticleSt = { revisions :: RevisionState }
type ArticleEf e = (trace :: Trace, routing :: R.Routing, ws :: WS.WebSocket | e)


data RevisionState 
  = RevisionAwaited
  | RevisionFetchFailed 
  | RevisionLoaded Revisions


data ArticleEv = InitFetch


-- articlePage :: forall e. T.Spec (T.Action (ArticleEf e) ArticleSt) ArticleSt ArticlePs ArticleEv
articlePage = T.simpleSpec initialState performAction render
            # T.componentWillMount InitFetch where

  initialState = { revisions: RevisionAwaited }

  performAction props action = 
    case action of
      InitFetch -> 
        T.asyncSetState \callback -> do
          let request = (QStartRevisions props.article)
          MS.send props.store (socketCallback callback) request


  --
  -- handles socket callback
  --
  socketCallback setState response = case response of

    WikiResponseR resp -> case resp of
      ARevisions name revisions -> do
        trace ("showing revisions for " ++ name ++ "...") 
        setState { revisions: RevisionLoaded revisions }

      _ -> do
        trace "for show reason the response was not revisions..."
        setState { revisions: RevisionFetchFailed } 

    WikiResponseE error -> case error of
      InternalError message -> do
        trace ("request failed because " ++ show message)
        setState { revisions: RevisionFetchFailed } 


  render _ state props = T.div [A._id "app", A.className "container"] 
    case state.revisions of 
      RevisionAwaited ->
        [T.p' [T.text ("Awaiting articles for " ++ props.article)]]

      RevisionFetchFailed ->
        [T.p' [T.text ("Could not load revisions for " ++ props.article)]]

      RevisionLoaded (Revisions cont revisions) ->
        [T.p' [T.text ("Now viewing article for " ++ props.article)]]



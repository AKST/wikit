module Components.Article (

  articlePage, 
  ArticlePs(), 
  ArticleSt(), 
  ArticleEv(), 
  ArticleEf(..)

) where

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

import Model.API


data ArticleEv = ArticleEv
type ArticlePs = { store :: MS.MessageStore, article :: String }
type ArticleSt = {}
type ArticleEf e = (trace :: Trace, routing :: R.Routing, ws :: WS.WebSocket | e)


articlePage :: forall e. T.Spec (T.Action (ArticleEf e) ArticleSt) ArticleSt ArticlePs ArticleEv
articlePage = T.simpleSpec {} performAction render where

  performAction _ _ = return unit

  render _ _ _ = T.p' [T.text "empty page"]


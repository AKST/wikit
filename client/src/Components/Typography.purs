module Components.Typography where

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T


header text className = T.header [A.className "header"] [
  T.h1 [A.className className] [T.text text]
] 


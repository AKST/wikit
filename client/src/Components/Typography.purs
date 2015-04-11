module Components.Typography where

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

import Data.Array


header text links = T.header [A.className "header"] [
    T.h1 [A.className "page-heading u-pull-left"] [T.text text],
    T.nav [A.className "navigation u-pull-right"] [
      T.ul [A.className "navigation-list"] (iconify <$> links)]] 

  where 

    iconify { iconName: i, url: url } =
      T.li [A.className "navigation-item"] [
        T.a [A.href url, A.className ("navigation-link")] [
          T.img [
            A.className "navigation-icon", 
            A.src ("/svg/" ++ i ++ ".svg")
          ] []
        ]
      ]



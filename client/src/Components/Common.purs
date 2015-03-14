module Components.Common where

import qualified Thermite.Events as T


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


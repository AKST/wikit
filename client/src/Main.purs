module Main where


import Control.Monad.Eff
import qualified Control.Monad.JQuery as J

import qualified Thermite as T

import Components.App


main = J.ready do
  T.render app {}



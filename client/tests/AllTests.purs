module AllTests where

import qualified Debug.Trace as Trace

import Control.Monad.Eff

import TestCommon


main = do
  Trace.trace "Running tests"
  quitTests


foreign import quitTests """
  function quitTests() {
    phantom.exit();
  }
  """ :: forall e. Eff e Unit


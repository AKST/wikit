module AllTests where

import qualified Debug.Trace as Trace

import Control.Monad.Eff

import TestCommon
import qualified Control.Monad.JQuery as J

import qualified Data.RevisionTest as Data.RevisionTest 
import qualified Data.APITests as Data.APITests
import qualified Text.WikiText.TokenTests as Text.WikiText.TokenTests
import qualified Text.WikiText.ParserTests as Text.WikiText.ParserTests
import qualified Text.DateTests as Text.DateTests


main = J.ready do
  initMocha
  Text.DateTests.tests
  Data.RevisionTest.tests
  Data.APITests.tests
  Text.WikiText.TokenTests.tests
  Text.WikiText.ParserTests.tests
  runMocha



foreign import initMocha """
  var initMocha = mocha.setup.bind(mocha, 'bdd');
  """ :: forall e. Eff e Unit

foreign import runMocha """
  var runMocha = (function () {
    var context = (window.mochaPhantomJS || window.mocha);
    return context.run.bind(context); 
  }());
  """ :: forall e. Eff e Unit


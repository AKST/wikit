module Text.WikiTextTests (tests) where


import qualified Data.Date as Date
import qualified Data.Either as Either
import Data.WikiText

import Text.WikiText

import Control.Monad.Eff

import Test.Mocha
import Test.Assert.Simple

import TestCommon


--
-- Spec:
--   http://www.mediawiki.org/wiki/Markup_spec
--
tests = do
  describe "Text.WikiText" do
    describe "Any Text" $ do
      it "hello" $ do
        result <- parseOrFail wikitextParser "hello" 
        result @=? AnyText { body: "hello" }



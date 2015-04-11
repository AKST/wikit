module Text.WikiTextTests (tests) where


import qualified Data.Date as Date
import qualified Data.Either as Either
import Data.WikiText
import Data.TextFormat

import qualified Text.WikiText as Parser

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
    describe "tokens" do
      describe "Any Text" $ do
        it "hello" $ do
          result <- parseOrFail Parser.wikitext "hello" 
          AnyText "hello" @=? result

        it "detects end of any text" do
          result <- parseOrFail Parser.article "hello '''''world'''''"
          [

            AnyText "hello ", 
            FormatText ItalicBold "world"

          ] @=? result

      describe "Format Text" do
        it "''hello''" do
          result <- parseOrFail Parser.wikitext "''hello''"
          FormatText Italic "hello" @=? result

        it "'''hello'''" do
          result <- parseOrFail Parser.wikitext "'''hello'''"
          FormatText Bold "hello" @=? result

        it "'''''hello'''''" do
          result <- parseOrFail Parser.wikitext "'''''hello'''''"
          FormatText ItalicBold "hello" @=? result

      


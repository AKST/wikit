module Text.WikiTextTests (tests) where


import qualified Data.Date as Date
import qualified Data.Either as Either
import Data.WikiText
import Data.TextFormat
import Data.Maybe

import qualified Text.WikiText as Parser

import Control.Monad.Eff

import Test.Mocha
import Test.Assert.Simple

import qualified TestCommon as Test


--
-- Spec:
--   http://www.mediawiki.org/wiki/Markup_spec
--
tests = do
  describe "Text.WikiText" do
    describe "Tokens" do
      describe "BodyText containing" $ do
        it "hello" $ do
          result <- Test.parseOrFail Parser.wikitext "hello" 
          Text (PlainText "hello") @=? result

        it "''hello'' (italic text)" do
          result <- Test.parseOrFail Parser.wikitext "''hello''"
          Text (FormatText Italic "hello") @=? result

        it "'''hello''' (bold text)" do
          result <- Test.parseOrFail Parser.wikitext "'''hello'''"
          Text (FormatText Bold "hello") @=? result

        it "'''''hello''''' (italic bold text)" do
          result <- Test.parseOrFail Parser.wikitext "'''''hello'''''"
          Text (FormatText ItalicBold "hello") @=? result

        it "[[Hello World|hello]] (Internal Link)" do
          result <- Test.parseOrFail Parser.wikitext "[[Hello World|hello]]"
          Text (Link Internal "Hello World" (Just "hello")) @=? result

        it "[[[Hello World|hello]]] (External link)" do
          result <- Test.parseOrFail Parser.wikitext "[[[Hello World|hello]]]"
          Text (Link External "Hello World" (Just "hello")) @=? result


      describe "Line Break" do
        it "\n" do
          result <- Test.parseOrFail Parser.wikitext "\n"
          LineBreak @=? result 

      describe "headings" do
        it "=hello=" do
          result <- Test.parseOrFail Parser.wikitext "\n=hello="
          Heading 1 "hello" @=? result

        it "==hello==" do
          result <- Test.parseOrFail Parser.wikitext "\n==hello=="
          Heading 2 "hello" @=? result

        it "===hello===" do
          result <- Test.parseOrFail Parser.wikitext "\n===hello==="
          Heading 3 "hello" @=? result

        it "====hello====" do
          result <- Test.parseOrFail Parser.wikitext "\n====hello===="
          Heading 4 "hello" @=? result

        it "=====hello=====" do
          result <- Test.parseOrFail Parser.wikitext "\n=====hello====="
          Heading 5 "hello" @=? result

        it "======hello======" do
          result <- Test.parseOrFail Parser.wikitext "\n======hello======"
          Heading 6 "hello" @=? result
      


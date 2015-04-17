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
          --
          -- italic bold text
          --
          result <- Test.parseOrFail Parser.wikitext "'''''hello'''''"
          Text (FormatText ItalicBold "hello") @=? result

        it "[[Hello World|hello]] (Internal Link)" do
          --
          -- internal link
          --
          result <- Test.parseOrFail Parser.wikitext "[[Hello World|hello]]"
          Text (Link Internal "Hello World" (Just "hello")) @=? result

        it "[[[Hello World|hello]]] (External link)" do
          --
          -- external link
          --
          result <- Test.parseOrFail Parser.wikitext "[[[Hello World|hello]]]"
          Text (Link External "Hello World" (Just "hello")) @=? result


      describe "content links" do
        it "[[File:Van Gogh.jpg|thumb|''Vincent's Chair'' by [[Vincent van Gogh]]]] (Image)" $ do
          --
          -- file link
          --
          result <- Test.parseOrFail Parser.wikitext
            "[[File:Van Gogh.jpg|thumb|''Vincent's Chair'' by [[Vincent van Gogh]]]]"

          Media File "Van Gogh.jpg" ["thumb"] [
            FormatText Italic "Vincent's Chair",
            PlainText " by ",
            Link Internal "Vincent van Gogh" Nothing
          ] @=? result

        it "[[File:SalisChair.jpg|thumb|left|upright|Early 20th century chair]]" do
          --
          -- file link with descriptors
          --
          result <- Test.parseOrFail Parser.wikitext
            "[[File:SalisChair.jpg|thumb|left|upright|Early 20th century chair]]"

          Media File "SalisChair.jpg" ["thumb", "left", "upright"] [
            PlainText "Early 20th century chair"
          ] @=? result


      describe "Line Breaks" do
        it "\\n" do
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
      

      describe "Template" do
        it "{{hello_world}}" do
          result <- Test.parseOrFail Parser.wikitext "{{hello_world}}"
          Template "hello_world" [] @=? result

        it "{{hello|world}}" do
          result <- Test.parseOrFail Parser.wikitext "{{hello|world}}"
          Template "hello" [PlainArg (PlainText "world")] @=? result 

        it "{{hello|to=world}}" do
          result <- Test.parseOrFail Parser.wikitext "{{hello|to=world}}"
          Template "hello" [NamedArg "to" (PlainText "world")] @=? result 




module Text.WikiText.TokenTests (tests) where

import Test.Mocha
import Test.Assert.Simple

import Data.WikiText.Tokens
import Data.TextFormat
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Text.WikiText.Tokens as Parser
import qualified TestCommon as Test

--
-- Spec:
--   http://www.mediawiki.org/wiki/Markup_spec
--
tests = do
  describe "Text.WikiText.Tokens" do
    describe "whitespace" do
      it " " do
        result <- Test.parseOrFail Parser.tokens " " 
        [Space] @=? result

      it "  " do
        result <- Test.parseOrFail Parser.tokens "  "
        [Space, Space] @=? result

    describe "linebreaks" do
      it "\\n" do
        result <- Test.parseOrFail Parser.tokens "\n"
        [Linebreak] @=? result

    describe "words" do
      it "hello world" do
        result <- Test.parseOrFail Parser.tokens "hello world"
        [Word "hello", Space, Word "world"] @=? result

    describe "punctuation" $ do
      it "." do
        result <- Test.parseOrFail Parser.tokens "."
        [Punctuation PPeroid] @=? result

      it "hello, world!" do
        result <- Test.parseOrFail Parser.tokens "hello, world!"
        [
          Word "hello", 
          Punctuation PComma, 
          Space, 
          Word "world", 
          Punctuation PExclaim
        ] @=? result

    describe "delimters" do
      it "'' ''' '''''" do
        result <- Test.parseOrFail Parser.tokens "'' ''' '''''"
        [
          AmbigiousDelimiter (DeFormat Italic),
          Space, 
          AmbigiousDelimiter (DeFormat Bold),
          Space, 
          AmbigiousDelimiter (DeFormat ItalicBold)
        ] @=? result

      it "[[[ [[ {{{ {{" do
        result <- Test.parseOrFail Parser.tokens "[[[ [[ {{{ {{"
        [
          OpeningDelimiter DeXLink,
          Space,
          OpeningDelimiter DeLink,
          Space,
          OpeningDelimiter DeTempPar,
          Space,
          OpeningDelimiter DeTemp
        ] @=? result

      it "====== ===== ==== === == =" do
        result <- Test.parseOrFail Parser.tokens "====== ===== ==== === == ="
        [
          ClosingDelimiter (DeHeading 6),
          Space,
          ClosingDelimiter (DeHeading 5),
          Space,
          ClosingDelimiter (DeHeading 4),
          Space,
          ClosingDelimiter (DeHeading 3),
          Space,
          ClosingDelimiter (DeHeading 2),
          Space,
          Ambigious (Set.fromList [
            ClosingDelimiter (DeHeading 1), 
            NamedParameterAssignment
          ])
        ] @=? result

      it "\\n====== \\n===== \\n==== \\n=== \\n== \\n=" do
        result <- Test.parseOrFail Parser.tokens "\n====== \n===== \n==== \n=== \n== \n="
        [
          OpeningDelimiter (DeHeading 6),
          Space,
          OpeningDelimiter (DeHeading 5),
          Space,
          OpeningDelimiter (DeHeading 4),
          Space,
          OpeningDelimiter (DeHeading 3),
          Space,
          OpeningDelimiter (DeHeading 2),
          Space,
          OpeningDelimiter (DeHeading 1)
        ] @=? result

    describe "xml" do
      it "<ref>hello</ref>" do
        result <- Test.parseOrFail Parser.tokens "<ref>hello</ref>"
        [
          Xml (Opening "ref"),
          Word "hello",
          Xml (Closing "ref")
        ] @=? result
        
      it "<ref/>" do
        result <- Test.parseOrFail Parser.tokens "<ref/>"
        [Xml (SelfClosing "ref")] @=? result

      it "<ref />" do
        result <- Test.parseOrFail Parser.tokens "<ref />"
        [Xml (SelfClosing "ref")] @=? result


      it "< ref ></ ref >" do
        result <- Test.parseOrFail Parser.tokens "< ref ></ ref >"
        [Xml (Opening "ref"), Xml (Closing "ref")] @=? result


      -- it "<ref name=\"john\"></ref>" do
      --   result <- Test.parseOrFail Parser.tokens "<ref name=\"john\"></ref>"
      --   [Xml (SelfClosing "ref")] @=? result


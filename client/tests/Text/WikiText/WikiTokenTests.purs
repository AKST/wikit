
module Text.WikiText.TokenTests (tests) where

import Control.Monad.Eff

import Test.Mocha
import Test.Assert.Simple

import Data.WikiText.Tokens
import Data.TextFormat
import Data.Tuple
import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified Text.WikiText.Tokens as Parser
import qualified TestCommon as Test


tokens :: String -> Eff _ [WikiToken]
tokens = Test.parseOrFail Parser.tokens


--
-- Spec:
--   http://www.mediawiki.org/wiki/Markup_spec
--
tests = do
  describe "Text.WikiText.Tokens" do
    describe "whitespace" do
      it " " do
        result <- tokens " " 
        result @?= [Space, EndOfInput]

      it "  " do
        result <- tokens "  "
        result @?= [Space, Space, EndOfInput]

    describe "linebreaks" do
      it "\\n" do
        result <- tokens "\n"
        result @?= [Linebreak, EndOfInput]

    describe "words" do
      it "hello world" do
        result <- tokens "hello world"
        result @?= [Word "hello", Space, Word "world", EndOfInput]

    describe "punctuation" $ do
      it "." do
        result <- tokens "."
        result @?= [Punctuation PPeroid, EndOfInput]

      it "hello, world!" do
        result <- tokens "hello, world!"
        result @?= [
          Word "hello", 
          Punctuation PComma, 
          Space, 
          Word "world", 
          Punctuation PExclaim, 
					EndOfInput
        ]

    describe "delimters" do
      it "'' ''' '''''" do
        result <- tokens "'' ''' '''''"
        result @?= [
          AmbigiousDelimiter (DeFormat Italic),
          Space, 
          AmbigiousDelimiter (DeFormat Bold),
          Space, 
          AmbigiousDelimiter (DeFormat ItalicBold), 
					EndOfInput
        ]

      it "[[[ [[ {{{ {{" do
        result <- tokens "[[[ [[ {{{ {{"
        result @?= [
          OpeningDelimiter DeXLink,
          Space,
          OpeningDelimiter DeLink,
          Space,
          OpeningDelimiter DeTempPar,
          Space,
          OpeningDelimiter DeTemp, 
					EndOfInput
        ]

      it "====== ===== ==== === == =" do
        result <- tokens "====== ===== ==== === == ="
        result @?= [
          AmbigiousDelimiter (DeHeading 6), 
          Space,
          AmbigiousDelimiter (DeHeading 5), 
          Space,
          AmbigiousDelimiter (DeHeading 4), 
          Space,
          AmbigiousDelimiter (DeHeading 3), 
          Space,
          AmbigiousDelimiter (DeHeading 2), 
          Space,
          Ambigious [
            AmbigiousDelimiter (DeHeading 1), 
            NamedParameterAssignment
          ], 
					EndOfInput
        ]

    describe "xml" do
      it "<ref>hello</ref>" do
        result <- tokens "<ref>hello</ref>"
        result @?= [
          Xml (Opening "ref" (Map.fromList [])),
          Word "hello",
          Xml (Closing "ref"), 
					EndOfInput
        ]
        
      it "<ref/>" do
        result <- tokens "<ref/>"
        result @?= [Xml (SelfClosing "ref" (Map.fromList [])), EndOfInput]

      it "<ref />" do
        result <- tokens "<ref />"
        result @?= [Xml (SelfClosing "ref" (Map.fromList [])), EndOfInput]


      it "< ref ></ ref >" do
        result <- tokens "< ref ></ ref >"
        result @?= [
					Xml (Opening "ref" (Map.fromList [])), 
					Xml (Closing "ref"), 
					EndOfInput
				]


      it "<ref name=\"john\"></ref>" do
        result <- tokens "<ref name=\"john\"></ref>"
        result @?= [
          Xml (Opening "ref" (Map.fromList [
            Tuple "name" "john"
          ])),
          Xml (Closing "ref"), 
					EndOfInput
        ]

      it "<ref name=\"john\"></ref>" do
        result <- tokens "<ref name=\"john\"/>"
        result @?= [
					Xml (SelfClosing "ref" (Map.fromList [
					  Tuple "name" "john"
					])), 
				  EndOfInput
				]


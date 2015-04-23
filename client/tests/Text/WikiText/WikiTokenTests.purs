
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
        result @?= [Space]

      it "  " do
        result <- tokens "  "
        result @?= [Space, Space]

    describe "linebreaks" do
      it "\\n" do
        result <- tokens "\n"
        result @?= [Linebreak]

    describe "words" do
      it "hello world" do
        result <- tokens "hello world"
        result @?= [Word "hello", Space, Word "world"]

    describe "punctuation" $ do
      it "." do
        result <- tokens "."
        result @?= [Punctuation PPeroid]

      it "hello, world!" do
        result <- tokens "hello, world!"
        result @?= [
          Word "hello", 
          Punctuation PComma, 
          Space, 
          Word "world", 
          Punctuation PExclaim
        ]

    describe "delimters" do
      it "'' ''' '''''" do
        result <- tokens "'' ''' '''''"
        result @?= [
          AmbigiousDelimiter (DeFormat Italic),
          Space, 
          AmbigiousDelimiter (DeFormat Bold),
          Space, 
          AmbigiousDelimiter (DeFormat ItalicBold)
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
          OpeningDelimiter DeTemp
        ]

      it "====== ===== ==== === == =" do
        result <- tokens "====== ===== ==== === == ="
        result @?= [
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
        ]

      it "\\n====== \\n===== \\n==== \\n=== \\n== \\n=" do
        result <- tokens "\n====== \n===== \n==== \n=== \n== \n="
        result @?= [
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
        ]

    describe "xml" do
      it "<ref>hello</ref>" do
        result <- tokens "<ref>hello</ref>"
        result @?= [
          Xml (Opening "ref" (Map.fromList [])),
          Word "hello",
          Xml (Closing "ref")
        ]
        
      it "<ref/>" do
        result <- tokens "<ref/>"
        result @?= [Xml (SelfClosing "ref" (Map.fromList []))]

      it "<ref />" do
        result <- tokens "<ref />"
        result @?= [Xml (SelfClosing "ref" (Map.fromList []))]


      it "< ref ></ ref >" do
        result <- tokens "< ref ></ ref >"
        result @?= [Xml (Opening "ref" (Map.fromList [])), Xml (Closing "ref")]


      it "<ref name=\"john\"></ref>" do
        result <- tokens "<ref name=\"john\"></ref>"
        result @?= [
          Xml (Opening "ref" (Map.fromList [
            Tuple "name" "john"
          ])),
          Xml (Closing "ref")
        ]

      it "<ref name=\"john\"></ref>" do
        result <- tokens "<ref name=\"john\"/>"
        result @?= [Xml (SelfClosing "ref" (Map.fromList [
          Tuple "name" "john"
        ]))]


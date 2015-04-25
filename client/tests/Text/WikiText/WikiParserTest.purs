module Text.WikiText.ParserTests (tests) where

import Control.Monad.Eff

import Data.WikiText
import Data.WikiText.Tokens

import qualified Text.WikiText.Parser as Parser
import Text.WikiText.Tokens

import Test.Mocha
import Test.Assert.Simple

import qualified TestCommon as Test


wikiText :: [WikiToken] -> Eff _ [WikiText]
wikiText = Test.parseOrFail Parser.wikiText


tests = do
  describe "Text.WikiText.Parser" do
    describe "empty" do
      it "results in empty doc" do
        doc <- wikiText [EndOfInput]
        doc @?= []

    describe "paragraph" do
      it "hello" do
        doc <- wikiText [Word "hello", EndOfInput]
        doc @?= [Paragraph [WordAtom "hello"]]

      it "hello world" do
        doc <- wikiText [
          Word "hello", Space, 
          Word "world", EndOfInput]
        doc @?= [Paragraph [WordAtom "hello", WordAtom "world"]]

    describe "heading" do
      it "standalone" do
        doc <- wikiText [
          Linebreak,
          AmbigiousDelimiter (DeHeading 6),
          Word "hello", Space, Word "world",
          AmbigiousDelimiter (DeHeading 6), 
          EndOfInput
        ]
        doc @?= [Heading 6 [
          WordAtom "hello", WordAtom "world"
        ]]
        


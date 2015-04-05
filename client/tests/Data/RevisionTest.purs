module Data.RevisionTest where
  
import qualified Data.Date as Date
import qualified Data.Either as Either

import Data.Revision
import Control.Monad.Eff

import Test.Chai
import Test.Mocha
import Test.Assert.Simple

import TestCommon


tests = do
  describe "Revisions" do
    it "decode no revision" do
      revisions <- parse """{
        "continue": 0, 
        "revisions": []
      }"""
      revisions @=? Revisions 0 []

  describe "Revision" do
    it "decode empty revision" do
      date <- fromMaybe "couldn't parse date"
        (Date.fromString "1970-01-01T00:00:00.000Z")
      revision <- parse """{
        "timestamp": "1970-01-01T00:00:00.000Z", 
        "wikitext": ""
      }"""
      revision @=? Revision date ""



module Data.RevisionTest where
  
import qualified Data.Date as Date
import qualified Data.Either as Either

import Data.Revision
import Control.Monad.Eff

import Test.Mocha
import Test.Assert.Simple

import TestCommon


tests = do

  let emptyRevision = """{
    "timestamp": "1970-01-01T00:00:00.000Z", 
    "wikitext": ""
  }"""
  let emptyRevisions = """{
    "continue": 0, 
    "revisions": []
  }"""

  let epochStart = Date.fromString "1970-01-01T00:00:00.000Z"

  describe "Data.Revisions" do
    describe "Revisions" do
      it "decode no revision" do
        revisions <- decodeOrFail emptyRevisions
        revisions @=? Revisions 0 []

    describe "Revision" do
      it "decode empty revision" do
        date <- fromMaybe "date parse error" epochStart
        revision <- decodeOrFail emptyRevision 
        revision @=? Revision date ""



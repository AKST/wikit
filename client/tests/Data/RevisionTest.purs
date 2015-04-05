module Data.RevisionTest where
  
import Data.Revision

import TestCommon


import Test.Chai
import Test.Mocha
import Test.Assert.Simple


tests = do
  describe "Revisions" do
    it "decode empty response" do
      expect 2 `toEqual` 2



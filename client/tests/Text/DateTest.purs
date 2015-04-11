module Text.DateTests (tests) where

import qualified Data.Date as Date

import qualified Text.Date as Text

import Control.Monad.Eff

import Test.Mocha
import Test.Assert.Simple

import TestCommon


tests = do
  let epochStart = Date.fromString "1970-01-01T00:00:00.000Z"

  describe "Text.Date" do
    describe "Date formatting" do
      it "1/1/1970" do
        date <- fromMaybe "date parse error" epochStart
        "1/1/1970" @=? (Text.ddmmyyDate date)

    describe "Time formatting" do
      it "00:00:00" do
        date <- fromMaybe "date parse error" epochStart
        "00:00:00" @=? (Text.hhmmssTime date)


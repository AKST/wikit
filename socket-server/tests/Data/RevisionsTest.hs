{-# LANGUAGE OverloadedStrings #-}

module Data.RevisionsTest where


import Control.Applicative
import Control.Monad

import Data.Revisions
import Data.Text (Text)
import Data.Word
import Data.Aeson
import Data.Time.Clock
import Data.Time.Calendar

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import TestCommon


tests :: [Test]
tests = [encodesRevision, encodesRevisions]


encodesRevision = testCase "revision encodes" $ do
  let epochStart = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)
  toJSON (Revision epochStart "") @=? object [
      "timestamp" .= epochStart,
      "wikitext"  .= ("" :: String) 
    ]

encodesRevisions = testCase "article revisions encode" $ do
  toJSON (ArticleRevisions 0 "hello" []) @=? object [
      "name" .= ("hello" :: String),
      "revisions" .= object [
        "continue" .= (0 :: Int),
        "revisions" .= ([] :: [Revision])
      ]
    ]



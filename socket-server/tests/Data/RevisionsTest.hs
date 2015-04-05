{-# LANGUAGE OverloadedStrings #-}

module Data.RevisionsTest where


import Control.Applicative
import Control.Monad

import Data.Revisions
import Data.Text (Text)
import Data.Word
import Data.Aeson
import Data.Time.Clock

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))

tests :: [Test]
tests = []

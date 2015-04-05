module Main where

import Test.HUnit (Assertion, (@=?), runTestTT, Test(..), Counts(..))
import System.Exit (ExitCode(..), exitWith)

import qualified Data.RevisionsTest as RevisionsTest


main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList RevisionsTest.tests ]


exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess




module Data.APITests (tests) where

import qualified Data.Date as Date
import qualified Data.Either as Either

import Data.API
import Data.Revision
import Control.Monad.Eff

import Test.Chai
import Test.Mocha
import Test.Assert.Simple

import TestCommon



tests = do

	let helloExists = """{
		"status":"ok",
		"body": {
			"type":"check",
			"contents": {
				"name":"hello",
				"exists": true
			}
		}
	}"""

	let helloRevisions = """{
		"status":"ok",
		"body": {
			"type": "revisions",
			"contents": {
				"name":"hello",
				"revisions": {
					"continue": 0,
					"revisions": []
				}
			}
		}
	}"""

	describe "Exists API" do
		it "decodes exists response" do
			exists <- decodeOrFail helloExists
			exists @=? (WikiResponseR (AArticleExist "hello" true))  	

	describe "Revisions API" do
		it "decodes revisions response" do
			revisions <- decodeOrFail helloRevisions
			revisions @=? (WikiResponseR 
				(ARevisions "hello" (Revisions 0 [])))  	


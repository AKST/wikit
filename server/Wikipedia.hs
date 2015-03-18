{-# LANGUAGE OverloadedStrings #-}


module Wikipedia where

import Prelude hiding (log)

import Network.URI
import Network.HTTP.Base

import System.Log (Priority(INFO))

import Connection 

import Data.Word
import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock

import qualified Data.HashMap.Strict as HM

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Control.Applicative
import Control.Monad
import Control.Monad.Except

import Common


data RevisionQuery = RevisionQuery {
  rvlimit    :: Word32,
  rvcontinue :: Maybe Word32,
  article    :: Text
}


revisionQuery = RevisionQuery {
  rvlimit    = 15,
  rvcontinue = Nothing,
  article    = ""
}


wikipediaURI = URI {
  uriScheme    = "http",
  uriAuthority = Just URIAuth {
    uriUserInfo = "",
    uriRegName  = "en.wikipedia.org",
    uriPort     = ""
  },
  uriPath      = "/w/api.php",
  uriQuery     = "",
  uriFragment  = ""
}


getRevisions :: RevisionQuery -> Conn RevisionRes
getRevisions query = do

  log INFO $ case rvcontinue query of
    Nothing -> "fetching the revisions for \"" ++ T.unpack (article query) ++ "\""
    Just ct -> "continuing the revisions for \"" ++ T.unpack (article query) ++ "\", from " ++ show ct

  result <- get (wikipediaURI { uriQuery = revisionQuery })

  unless (isOk result) $
    throwError (WikiResponseNotOk result) 

  case decode (rspBody result) of
    Nothing      -> throwError (CouldNotParseArticle (rspBody result))
    Just article -> return article     

  where
    isOk (Response { rspCode = (2, _, _)}) = True
    isOk _________________________________ = False

    revisionQuery = baseQuery ++ case rvcontinue query of
      Just continue -> "?rvcontinue=" ++ show continue
      Nothing       -> ""

    baseQuery = "?action=query"
             ++ "&prop=revisions"
             ++ "&titles=" ++ (urlEncode (T.unpack (article query)))
             ++ "&rvprop=content|timestamp"
             ++ "&rvlimit=" ++ show (rvlimit query)
             ++ "&format=json"


-- http://en.wikipedia.org/w/api.php
--   ?action=query
--   &prop=revisions
--   &titles=Adolf%20Hitler
--   &rvprop=content|timestamp
--   &rvlimit=2
--   &format=JSON



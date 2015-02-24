{-# LANGUAGE OverloadedStrings #-}


module Wikipedia where


import Network.URI
import Network.HTTP.Base

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

import Connection
import API

-- http://en.wikipedia.org/w/api.php
--   ?action=query
--   &prop=revisions
--   &titles=Adolf%20Hitler
--   &rvprop=content|timestamp
--   &rvlimit=2
--   &format=JSON

data ArticleRevisions = ArticleRevisions {
  revisionContinue :: Word32,
  title  :: Text,
  pageId :: Word32,
  pages :: [Revision]  
}

data Revision = Revision {
  timestamp :: UTCTime,
  contentFormat :: Text,
  contentModel :: Text,
  revisionBody :: Text
}

instance FromJSON ArticleRevisions where
  parseJSON (Object v) =
    ArticleRevisions <$> ((v .: "query-continue") >>= (.: "revisions") >>= (.: "rvcontinue")) 
                     <*> fromArticle "title"
                     <*> fromArticle "pageid"
                     <*> fromArticle "revisions"

    where fromArticle key = do
            articles <- (v .: "query") >>= (.: "pages")
            case HM.elems (articles :: HM.HashMap Text Value) of 
              Object v:_ -> v .: key
              _   -> mzero

  parseJSON _ = mzero
    

instance FromJSON Revision where
  parseJSON (Object v) =
    Revision <$> v .: "timestamp"
             <*> v .: "contentformat" 
             <*> v .: "contentmodel"
             <*> v .: "*"
  parseJSON _ = mzero
    


data RevisionQuery = RevisionQuery {
  rvlimit    :: Word32,
  rvcontinue :: Maybe Word32,
  article    :: Text
}


wikiQuery = RevisionQuery {
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


getRevisions :: RevisionQuery -> Conn ArticleRevisions   
getRevisions query = do
  result <- get (wikipediaURI { uriQuery = wikiQuery })

  unless (isOk result) $
    throwError (WikiResponseNotOk result) 

  case decode (rspBody result) of
    Nothing      -> throwError (CouldNotParseArticle (rspBody result))
    Just article -> return article     

  where
    isOk (Response { rspCode = (2, _, _)}) = True
    isOk _________________________________ = False

    wikiQuery = baseQuery ++ case rvcontinue query of
      Just continue -> "?rvcontinue=" ++ show continue
      Nothing       -> ""

    baseQuery = "?action=query"
             ++ "&prop=revisions"
             ++ "&titles=" ++ (urlEncode (show (article query)))
             ++ "&rvprop=content|timestamp"
             ++ "&rvlimit=" ++ show (rvlimit query)
             ++ "&format=JSON"



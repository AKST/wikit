{-# LANGUAGE OverloadedStrings #-}

module Common (
  ConnError(FailedConnection, WikiResponseNotOk, CouldNotParseArticle),
  FatalError(UnknownFatalError, CouldntParseRequest),
  ArticleExists(ArticleExists),
  RevisionRes(RevisionRes),
  ExistsRes(ExistsRes),

) where


import qualified Network.HTTP   as HTTP
import qualified Network.Stream as HTTP

import Data.Revisions
import Data.Word
import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock
import Data.ByteString.Lazy
import qualified Data.HashMap.Strict as HM

import Control.Monad
import Control.Applicative


-- # connection monad errors

data ConnError 
  = FailedConnection HTTP.ConnError
  | WikiResponseNotOk (HTTP.Response ByteString)
  | CouldNotParseArticle ByteString
  deriving Show

data FatalError
  = UnknownFatalError
  | CouldntParseRequest

--
-- Article Exists
--
data ArticleExists = ArticleExists Text Bool

--
-- Revision Response from wikipedia
--
newtype RevisionRes = RevisionRes ArticleRevisions 
newtype ExistsRes = ExistsRes ArticleExists 


instance FromJSON RevisionRes where
  parseJSON (Object v) = do
    article <- getArticle v
    revisions <- ArticleRevisions 
      <$> ((v .: "query-continue") 
         >>= (.: "revisions") 
         >>= (.: "rvcontinue")) 
      <*> article .: "title"
      <*> article .: "pageid"
      <*> article .: "revisions"
    return (RevisionRes revisions)
  parseJSON _ = mzero

instance FromJSON ExistsRes where
  parseJSON (Object v) = do
    article <- getArticle v
    missing <- article .:? "missing"
    title   <- article .: "title"
    return $ ExistsRes $ case (missing :: Maybe Text) of
      Just _  -> ArticleExists title False
      Nothing -> ArticleExists title True
  parseJSON _ = mzero
                      

    
getArticle v = do
  articles <- (v .: "query") >>= (.: "pages")
  case HM.elems (articles :: HM.HashMap Text Value) of 
    Object v:_ -> return v
    _          -> mzero

fromArticle v key = do
  articles <- (v .: "query") >>= (.: "pages")
  case HM.elems (articles :: HM.HashMap Text Value) of 
    Object v:_ -> v .: key
    _          -> mzero


instance ToJSON ArticleExists where
  toJSON (ArticleExists name exists) =
    object [
      "name"   .= name,
      "exists" .= exists
    ]
    
instance ToJSON ConnError where
  toJSON error = object ["message" .= message] where

    message :: Text
    message = case error of
      _ -> "gateway error" 

instance ToJSON FatalError where
  toJSON error = object [
    "status" .= ("bad request" :: Text),
    "body" .= ["message" .= message]] 
    
    where

      message :: Text
      message = case error of
        UnknownFatalError   -> "Unhandled error :( sorry"
        CouldntParseRequest -> "Can you even json?"


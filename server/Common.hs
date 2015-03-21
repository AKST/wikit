{-# LANGUAGE OverloadedStrings #-}

module Common (
  ConnError(FailedConnection, WikiResponseNotOk, CouldNotParseArticle),
  ArticleExists(ArticleExists),
  RevisionRes(RevisionRes),
  ExistsRes(ExistsRes),

  ArticleRevisions(ArticleRevisions),
  Revision(Revision)
) where


import qualified Network.HTTP   as HTTP
import qualified Network.Stream as HTTP

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

--
-- Article Exists
--
data ArticleExists = ArticleExists Text Bool

--
-- Revision Response from wikipedia
--
newtype RevisionRes = RevisionRes ArticleRevisions 
newtype ExistsRes = ExistsRes ArticleExists 

--
-- Article Revision 
--
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
                      

    
instance FromJSON Revision where
  parseJSON (Object v) =
    Revision <$> v .: "timestamp"
             <*> v .: "contentformat" 
             <*> v .: "contentmodel"
             <*> v .: "*"
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


instance ToJSON ArticleRevisions where
  toJSON (ArticleRevisions c t id ps) = 
    object [
      "revisionContinue" .= c,
      "title"  .= t,
      "pageId" .= id,
      "pages"  .= ps
    ]

instance ToJSON ArticleExists where
  toJSON (ArticleExists name exists) =
    object [
      "name"   .= name,
      "exists" .= exists
    ]
    
instance ToJSON Revision where
  toJSON (Revision t cf cm rb) =
    object [
      "timestamp" .= t, 
      "contentformat" .= cf,
      "contentmodel" .= cm,
      "revisionBody" .= rb
    ]



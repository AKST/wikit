{-# LANGUAGE OverloadedStrings #-}

module Common where


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


instance ToJSON ArticleRevisions where
  toJSON (ArticleRevisions c t id ps) = 
    object [
      "revisionContinue" .= c,
      "title"  .= t,
      "pageId" .= id,
      "pages"  .= ps
    ]
    
instance ToJSON Revision where
  toJSON (Revision t cf cm rb) =
    object [
      "timestamp" .= t, 
      "contentformat" .= cf,
      "contentmodel" .= cm,
      "revisionBody" .= rb
    ]



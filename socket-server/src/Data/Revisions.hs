{-# LANGUAGE OverloadedStrings #-}

module Data.Revisions where


import Control.Applicative
import Control.Monad

import Data.Text (Text)
import Data.Word
import Data.Aeson
import Data.Time.Clock


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


instance FromJSON Revision where
  parseJSON (Object v) =
    Revision <$> v .: "timestamp"
             <*> v .: "contentformat" 
             <*> v .: "contentmodel"
             <*> v .: "*"
  parseJSON _ = mzero

instance ToJSON Revision where
  toJSON (Revision t cf cm rb) =
    object [
      "timestamp" .= t, 
      "wikitext" .= rb
    ]

instance ToJSON ArticleRevisions where
  toJSON (ArticleRevisions c t id ps) = 
    object [
      "name" .= t,
      "revisions" .= object [
        "continue"  .= c,
        "revisions" .= ps
      ]
    ]




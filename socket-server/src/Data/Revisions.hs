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
  pages :: [Revision]  
}

data Revision = Revision {
  timestamp :: UTCTime,
  revisionBody :: Text
}


instance FromJSON Revision where
  parseJSON (Object v) =
    Revision <$> v .: "timestamp"
             <*> v .: "*"
  parseJSON _ = mzero

instance ToJSON Revision where
  toJSON (Revision t b) =
    object [
      "timestamp" .= t, 
      "wikitext" .= b
    ]

instance ToJSON ArticleRevisions where
  toJSON (ArticleRevisions c t ps) = 
    object [
      "name" .= t,
      "revisions" .= object [
        "continue"  .= c,
        "revisions" .= ps
      ]
    ]




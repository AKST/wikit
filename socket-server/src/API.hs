{-# LANGUAGE OverloadedStrings #-}

module API where


import qualified Network.HTTP   as HTTP
import qualified Network.Stream as HTTP

import Data.Word
import Data.Aeson
import Data.Text (Text)

import Data.ByteString.Lazy

import Control.Monad
import Control.Applicative

import Common

-- # Format of incoming messages

data Request = Request WikiTReq Int

data WikiTReq 
  = WCheck Text
  | WStart Text
  | WCont Text Word32

-- # Responses

data Response 
  = Response WikiTRes Int
  | ErrorRes ConnError Int

data WikiTRes 
  = WEcho WikiTReq
  | WExists ArticleExists
  | WRevisions ArticleRevisions

-- # Serialisation

instance FromJSON Request where
  parseJSON (Object v) = do
    Request <$> parseJSON (Object v)
            <*> v .: "timestamp"

instance FromJSON WikiTReq where
  parseJSON (Object v) = do 
    rType <- v .: "type"
    name  <- v .: "name"
    case (rType :: Maybe Text) of
      Just "check"    -> pure (WCheck name)
      Just "start"    -> pure (WStart name)
      Just "continue" -> do
        continue <- v .: "continue"
        pure (WCont name continue)
      _ -> mzero

  parseJSON _ = mzero
                      
instance ToJSON WikiTReq where
  toJSON (WStart name) = 
    object [
      "type" .= ("start" :: Text), 
      "name" .= name
    ] 
  toJSON (WCheck name) = 
    object [
      "type" .= ("check" :: Text), 
      "name" .= name
    ]
  toJSON (WCont name c) = 
    object [
      "type" .= ("continue" :: Text), 
      "name" .= name, 
      "continue" .= c
    ]

instance ToJSON Response where
  toJSON (Response res id) = object [
    "timestamp" .= id,  
    "status"    .= ("ok" :: Text),
    "body"      .= toJSON res]
  toJSON (ErrorRes _ id) = object [
    "timestamp" .= id,  
    "status"    .= ("error" :: Text),
    "body"      .= object [
      "message" .= ("internal server error" :: Text)]]   

instance ToJSON WikiTRes where 
  toJSON res = object ["contents" .= body, "type" .= bodyType] where

    body = case res of 
      WEcho r      -> toJSON r
      WExists r    -> toJSON r
      WRevisions r -> toJSON r

    bodyType = case res of
      WEcho _      -> "echo" :: Text
      WExists _    -> "check" 
      WRevisions _ -> "revisions" 


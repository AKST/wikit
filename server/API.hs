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

data WikiTReq 
  = WCheck Text
  | WStart Text
  | WCont Text Word32

-- # Responses

data WikiTRes 
  = WEcho WikiTReq
  | WExists ArticleExists
  | WRevisions ArticleRevisions

data WikiTErr 
  = WParseError
  | WInternalError ConnError

-- # Serialisation

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

  parseJSON _ = mzero
                      
instance ToJSON WikiTReq where
  toJSON (WStart name) = 
    object ["name" .= name] 
  toJSON (WCont name c) = 
    object ["name" .= name, "continue" .= c]


instance ToJSON WikiTRes where 
  toJSON res = object [
      "status" .= status, 
      "body"   .= object [
        "contents" .= body,
        "type"     .= bodyType]] 
    where
      status = "ok" :: Text

      body = case res of 
        WEcho r      -> toJSON r
        WExists r    -> toJSON r
        WRevisions r -> toJSON r

      bodyType = case res of
        WEcho _      -> "echo" :: Text
        WExists _    -> "existential" 
        WRevisions _ -> "revisions" 

      

instance ToJSON WikiTErr where
  toJSON err = object ["status" .= status, "message" .= body] where  

    status = "error" :: Text
    
    body = case err of
      WParseError      -> "could not parse request" :: Text 
      WInternalError _ -> "internal error occured" 



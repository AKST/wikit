{-# LANGUAGE OverloadedStrings #-}

module API where


import qualified Network.HTTP   as HTTP
import qualified Network.Stream as HTTP

import Data.Word
import Data.Aeson
import Data.Text (Text)

import Data.ByteString.Lazy

import Control.Monad

import Common

-- # Format of incoming messages

data WikiTReq 
  = WStart Text
  | WRange Text Word32 Word32

-- # Responses

data WikiTRes 
  = WEcho WikiTReq
  | WRevisions ArticleRevisions

data WikiTErr 
  = WParseError
  | WInternalError ConnError

-- # Serialisation

instance FromJSON WikiTReq where
  parseJSON (Object v) = do 
    name <- v .:  "name"
    from <- v .:? "from"
    to   <- v .:? "to"
    return $ case (from, to) of
      (Just f, Just t) -> WRange name f t
      (_     , _     ) -> WStart name

  parseJSON _ = mzero
                      
instance ToJSON WikiTReq where
  toJSON (WStart name) = 
    object ["name" .= name] 
  toJSON (WRange name f t) = 
    object ["name" .= name, "from" .= f, "to" .= t]


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
        WRevisions r -> toJSON r

      bodyType = case res of
        WEcho _      -> "echo" :: Text
        WRevisions _ -> "revisions" 

      

instance ToJSON WikiTErr where
  toJSON err = object ["status" .= status, "message" .= body] where  

    status = "error" :: Text
    
    body = case err of
      WParseError      -> "could not parse request" :: Text 
      WInternalError _ -> "internal error occured" 



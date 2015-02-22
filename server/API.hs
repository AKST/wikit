{-# LANGUAGE OverloadedStrings #-}

module API where


import Data.Word
import Data.Aeson
import Data.Text (Text)


-- # Format of incoming messages

data WikiTReq = WStart Text
              | WRange Text Word32 Word32

-- # Responses

data WikiTRes = WEcho WikiTReq

data WikiTErr = WParseError

-- # Serialisation

instance FromJSON WikiTReq where
  parseJSON (Object v) = do 
    name <- v .:  "name"
    from <- v .:? "from"
    to   <- v .:? "to"
    return $ case (from, to) of
      (Just f, Just t) -> WRange name f t
      (_     , _     ) -> WStart name
                      
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

      status :: Text
      status = "ok"

      body = case res of 
        WEcho r -> toJSON r

      bodyType :: Text
      bodyType = case res of
        WEcho _ -> "echo"

      


instance ToJSON WikiTErr where
  toJSON err = object ["status" .= status, "msg" .= body] where  

    status :: Text
    status = "error"
    
    body :: Text
    body = case err of
      WParseError -> "could not parse request" 



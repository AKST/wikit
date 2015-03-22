module Network.WebSocketAPI where


import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Maybe (Maybe(..))


newtype WikiRequest  = WikiRequest { name :: String, continue :: Maybe Number } 
newtype WikiResponse = WikiResponse { status :: String } 


instance decodeWikiRequest :: DecodeJson WikiRequest where 
  decodeJson json = do
    obj <- decodeJson json
    name <- obj .? "name"
    cont <- obj .? "continue"
    pure (WikiRequest { name: name, continue: cont } )

instance decodeWikiResponse :: DecodeJson WikiResponse where
  decodeJson json = do
    object <- decodeJson json
    status <- object .? "status"
    pure (WikiResponse { status: status } )


instance encodeWikiRequest :: EncodeJson WikiRequest where
  encodeJson (WikiRequest obj) = 
    let withName = "name" := obj.name 
        withType = "type" := "check"
    in case obj.continue of
      Nothing   -> withName ~> withType ~> jsonEmptyObject
      Just cont -> withName ~> withType ~> "continue" := cont ~> jsonEmptyObject


instance encodeWikiResponse :: EncodeJson WikiResponse where
  encodeJson (WikiResponse obj) 
    =  "status" := obj.status
    ~> jsonEmptyObject




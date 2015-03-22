module Model.API where


import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Either (Either(Left))
import Data.Maybe (Maybe(..))



data WikiResponse 
  = WikiResponseR WikiResult
  | WikiResponseE WikiError

data WikiError 
  = InternalError String

data WikiResult 
  = AArticleExist { name :: String, exists :: Boolean }
  | ARevisions { name :: String }

data WikiRequest  
  = QArticleExists { name :: String } 
  | QStartRevisions { name :: String } 
  | QContinueRevisions { name :: String, continue :: Number } 



instance decodeWikiResponse :: DecodeJson WikiResponse where
  decodeJson json = do
    object <- decodeJson json
    status <- object .? "status"
    body   <- object .? "body"
    cont   <- body .? "contents"
    case status of
      "error" -> do
        msg <- cont .? "message"
        pure (WikiResponseE (InternalError msg))
      "ok" -> do
        kind <- body .? "type"
        name <- cont .? "name"
        result <- case kind of
          "check" -> do
            exists <- cont .? "exists"
            pure (AArticleExist { name: name, exists: exists }) 
          "revisions" -> do
            pure (ARevisions { name: name })
          _ ->
            Left ("\"" ++ kind ++ "\" is not a valid kind")
        pure (WikiResponseR result)
      _ -> Left ("\"" ++ status ++ "\" is not a valid status")


instance encodeWikiRequest :: EncodeJson WikiRequest where
  encodeJson (QArticleExists obj) 
    =  "name" := obj.name 
    ~> "type" := "check"
    ~> jsonEmptyObject
  encodeJson (QStartRevisions obj) 
    =  "name" := obj.name 
    ~> "type" := "start"
    ~> jsonEmptyObject
  encodeJson (QContinueRevisions obj) 
    =  "name"     := obj.name 
    ~> "continue" := obj.continue 
    ~> "type"     := "continue"
    ~> jsonEmptyObject




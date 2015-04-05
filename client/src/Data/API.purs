module Data.API where


import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Either (Either(Left))
import Data.Maybe (Maybe(..))
import Data.Revision 


data WikiResponse 
  = WikiResponseR WikiResult
  | WikiResponseE WikiError

data WikiError 
  = InternalError String

data WikiResult 
  = AArticleExist String Boolean
  | ARevisions String Revisions

data WikiRequest  
  = QArticleExists String 
  | QStartRevisions String
  | QContinueRevisions String Number



instance decodeWikiResponse :: DecodeJson WikiResponse where
  decodeJson json = do
    object <- decodeJson json
    status <- object .? "status"
    body   <- object .? "body"
    case status of
      "error" -> do
        msg <- body .? "message"
        pure (WikiResponseE (InternalError msg))
      "ok" -> do
        kind <- body .? "type"
        cont <- body .? "contents"
        name <- cont .? "title"
        result <- case kind of
          "check" -> do
            exists <- cont .? "exists"
            pure (AArticleExist name exists) 
          "revisions" -> do
            revisions <- body .? "contents"
            pure (ARevisions name revisions)
          _ ->
            Left ("\"" ++ kind ++ "\" is not a valid kind")
        pure (WikiResponseR result)
      _ -> Left ("\"" ++ status ++ "\" is not a valid status")


instance encodeWikiRequest :: EncodeJson WikiRequest where
  encodeJson (QArticleExists name) 
    =  "name" := name 
    ~> "type" := "check"
    ~> jsonEmptyObject
  encodeJson (QStartRevisions name) 
    =  "name" := name 
    ~> "type" := "start"
    ~> jsonEmptyObject
  encodeJson (QContinueRevisions name continue) 
    =  "name"     := name 
    ~> "continue" := continue 
    ~> "type"     := "continue"
    ~> jsonEmptyObject




module Data.API where


import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Either (Either(Left))
import Data.Maybe (Maybe(..))
import Data.Revision 

--
-- TODO make better names for these constructors
--

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
  --
  -- TODO make Number an opitional for the same constructor
  --
  | QStartRevisions String
  | QContinueRevisions String Number


-- DECODE INSTANCE

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
        name <- cont .? "name"
        result <- case kind of
          "check" -> do
            exists <- cont .? "exists"
            pure (AArticleExist name exists) 
          "revisions" -> do
            revisions <- cont .? "revisions"
            pure (ARevisions name revisions)
          _ ->
            Left ("\"" ++ kind ++ "\" is not a valid kind")
        pure (WikiResponseR result)
      _ -> Left ("\"" ++ status ++ "\" is not a valid status")

-- ENCODE INSTANCE

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

-- SHOW INSTANCE

instance showWikiRequest :: Show WikiRequest where
  show (QArticleExists n) = "QArticleExists (" ++ show n ++ ")"
  show (QStartRevisions n) = "QStartRevisions (" ++ show n ++ ")"
  show (QContinueRevisions n c) = 
    "QContinueRevisions " ++
    "(" ++ show n ++ ") " ++
    "(" ++ show c ++ ")"

instance showWikiResponse :: Show WikiResponse where
  show (WikiResponseR r) = "WikiResponseR (" ++ show r ++ ")"
  show (WikiResponseE e) = "WikiResponseE (" ++ show e ++ ")"

instance showWikiResult :: Show WikiResult where
  show (AArticleExist n b) = 
    "AArticleExist " ++
    "(" ++ show n ++ ") " ++
    "(" ++ show b ++ ")"
  show (ARevisions n r) = 
    "ARevisions " ++
    "(" ++ show n ++ ") " ++
    "(" ++ show r ++ ")"

instance showWikiError :: Show WikiError where
  show (InternalError m) = "InternalError (" ++ show m ++ ")"

-- EQ INSTANCE

instance eqWikiRequest :: Eq WikiRequest where
  (/=) r1 r2 = not (r1 == r2)

  (==) (QArticleExists n1)        (QArticleExists n2)        = n1 == n2
  (==) (QStartRevisions n1)       (QStartRevisions n2)       = n1 == n2
  (==) (QContinueRevisions n1 c1) (QContinueRevisions n2 c2) = 
    n1 == n2 && c1 == c2

  (==) _ _ = false

instance eqWikiResponse :: Eq WikiResponse where
  (/=) r1 r2 = not (r1 == r2)

  (==) (WikiResponseR r1) (WikiResponseR r2) = r1 == r2
  (==) (WikiResponseE e1) (WikiResponseE e2) = e1 == e2

  (==) _ _ = false

instance eqWikiResult :: Eq WikiResult where
  (/=) r1 r2 = not (r1 == r2)

  (==) (AArticleExist n1 b1) (AArticleExist n2 b2) = n1 == n2 && b1 == b2  
  (==) (ARevisions n1 r1)    (ARevisions n2 r2)    = r1 == r2 && n1 == n2

  (==) _ _ = false

instance eqWikiError :: Eq WikiError where
  (/=) e1 e2 = not (e1 == e2)

  (==) (InternalError s1) (InternalError s2) = s1 == s2

  (==) _ _ = false


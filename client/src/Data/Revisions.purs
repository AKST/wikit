module Data.Revision where


import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(..))
import Data.Date (Date(..))

import qualified Data.Maybe as Maybe
import qualified Data.Date as Date


data Revision  = Revision Date.Date String 
data Revisions = Revisions Number [Revision]


instance decodeRevision :: DecodeJson Revision where
  decodeJson json = do
    object <- decodeJson json
    body <- object .? "revisionBody"
    date <- object .? "timestamp" >>= parseDate
    return (Revision date body) where

    parseDate :: String -> Either String Date.Date
    parseDate dateString = 
      let nativeDate = Date.fromString dateString
      in Maybe.maybe (Left "couldn't parse date") Right nativeDate

instance decodeRevisions :: DecodeJson Revisions where
  decodeJson json = do
    object <- decodeJson json
    continue  <- object .? "revisionContinue"
    revisions <- object .? "pages"
    return (Revisions continue revisions)

instance eqRevision :: Eq Revision where
	(==) (Revision d1 c1) (Revision d2 c2) = d1 == d2 && c1 == c2 
	(/=) r1 r2 = not (r1 == r2) 

instance eqRevisions :: Eq Revisions where
	(==) (Revisions c1 p1) (Revisions c2 p2) = c1 == c2 && p1 == p2
	(/=) r1 r2 = not (r1 == r2) 


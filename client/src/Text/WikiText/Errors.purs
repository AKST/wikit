module Text.WikiText.Errors where

import Control.Monad.Error.Trans 
import Control.Monad.Error.Class
import Control.Monad.Error


data ParseError
	= Intern InternalError
	| String String
	| Unknown

data InternalError
	= RanOutOfTokens


instance errorParseError :: Error ParseError where 
	noMsg = Unknown
	strMsg = String
	
instance showParseError :: Show ParseError where
	show (Intern err) = "Text.WikiText.Errors.ParseError.Intern " ++ show err
	show (String str) = "Text.WikiText.Errors.ParseError.String " ++ show str
	show Unknown = "Text.WikiText.Errors.ParseError.Unknown"

instance showInternalError :: Show InternalError where
	show RanOutOfTokens = "Text.WikiText.Errors.InternalError.RanOutOfTokens"


module Text.WikiText (

	wikitextParser, 
  WikiTextParser(..)

) where


import Control.Alt
import Control.Alternative
import Control.Monad.Eff
import Control.Lazy

import Data.Monoid
import Data.WikiText
-- import Data.Identity

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token


--type WikiTextParser a = ParserT String Identity a 
type WikiTextParser a = Parser String a 


wikitextParser :: WikiTextParser WikiText
wikitextParser = anyText where

	anyText = do
		characters <- many char 
		pure (AnyText { body: concat characters })


-- untility


concat :: forall m. (Monoid m) => [m] -> m
concat elems = impl mempty elems where
	impl acc (x:xs) = impl (acc <> x) xs
	impl acc [    ] = acc


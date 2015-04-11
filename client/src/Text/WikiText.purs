module Text.WikiText (

  wikitext,
  article,
  WikiTextParser(..)

) where


import Control.Alt
import Control.Alternative
import Control.Apply
import Control.Monad.Eff
import Control.Lazy

import Data.TextFormat
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

article :: WikiTextParser [WikiText]
article = concat <$> (manyTill wikitext eof `sepBy` string "\n") <* eof


wikitext :: WikiTextParser WikiText
wikitext = formatText <|> anyText

  
anyText :: WikiTextParser WikiText
anyText = AnyText <$> (concat <$> manyTill char delimiter)


formatText :: WikiTextParser WikiText
formatText = italicsBold <|> bold <|> italics where
  bold        = format Bold "'''" 
  italics     = format Italic "''" 
  italicsBold = format ItalicBold "'''''" 

  format t sep = FormatText t <$> parser where
    parser  = concat <$> (divider *> manyTill char divider)
    divider = string sep 




delimiter :: Parser String Unit
delimiter = lookAhead (choice [
  vstring "'''''",
  eof
])

  
-- untility

vstring :: String -> Parser String Unit
vstring str = string str *> pure unit


anyCharacters :: Parser String String 
anyCharacters = concat <$> many char


concat :: forall m. (Monoid m) => [m] -> m
concat elems = impl mempty elems where
	impl acc (x:xs) = impl (acc <> x) xs
	impl acc [    ] = acc


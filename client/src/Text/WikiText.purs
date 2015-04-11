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
article = manyTill wikitext eof


wikitext :: WikiTextParser WikiText
wikitext = 
  (atStartOfLine <?> "at start of line") <|> 
  ((Text <$> bodyText) <?> "body text")

bodyText :: WikiTextParser BodyText
bodyText =
  (link <?> "link") <|> 
  (formatText <?> "format text") <|> 
  (anyText <?> "any text")

--
-- parser only at the start of a new line
--
atStartOfLine :: WikiTextParser WikiText
atStartOfLine = string "\n" *> ((heading <?> "headings") <|> (linebreak <?> "linebreak"))


--
-- new lines
--
linebreak :: WikiTextParser WikiText
linebreak = pure LineBreak


--
-- headings
--
-- - h1 = `=hello=`
-- - h2 = `==hello==`
-- - h3 = `===hello===`
-- - h4 = `====hello====`
-- - h5 = `=====hello=====`
-- - h6 = `======hello======`
--
heading :: WikiTextParser WikiText
heading = h6 <|> h5 <|> h4 <|> h3 <|> h2 <|> h1 where

  h1 = headingImpl 1 "="
  h2 = headingImpl 2 "=="
  h3 = headingImpl 3 "==="
  h4 = headingImpl 4 "===="
  h5 = headingImpl 5 "====="
  h6 = headingImpl 6 "======"

  headingImpl size sep = Heading size <$> parser where
    parser  = concat <$> (divider *> manyTill char divider)
    divider = string sep 


-- PLAIN TEXT


--
-- plain old text
--
anyText :: WikiTextParser BodyText
anyText = PlainText <$> (concat <$> manyTill char leftDelimiter)


--
-- Links to wiki articles && external articles
--
-- - internal link 
--   - `[[Hello]]`
--   - `[[Hello|hello]]`
-- - external link 
--   - `[[[http...]]]`
--   - `[[[http...|hello]]]`
--
link :: WikiTextParser BodyText
link = wrapper "[[[" "]]]" External
   <|> wrapper "[["  "]]"  Internal where   

  wrapper left right t = do 
    --
    -- left delimiter
    --
    string left 
    linkTarget  <- concat <$> manyTill char endOfLinkDelimiter
    --
    -- optional display text
    --
    displayText <- optionMaybe do
      string "|"
      concat <$> manyTill char (lookAhead (string right))
    --
    -- right delimiter
    --
    string right 
    pure (Link t linkTarget displayText) where

      --
      -- the delimiter at the end of the url can be either
      -- a `|` if there is defined display text, or the right
      -- link delimiter if there is no display text.
      --
      endOfLinkDelimiter = lookAhead
        (string "|" <|> string right) 

--
-- Format Text
--
-- - italics     = `''hello''`
-- - bold        = `'''hello'''`
-- - italic+bold = `'''''hello'''''`
--
formatText :: WikiTextParser BodyText
formatText = italicsBold <|> bold <|> italics where

  bold        = (format Bold "'''") <?> "bold"
  italics     = (format Italic "''") <?> "italic"
  italicsBold = (format ItalicBold "'''''") <?> "italic bold" 

  format t sep = FormatText t <$> parser where
    parser  = concat <$> (divider *> manyTill char divider)
    divider = string sep 


leftDelimiter :: Parser String Unit
leftDelimiter = lookAhead (choice [
  vstring "|",
  vstring "[[",
  vstring "]]",
  vstring "'''''",
  vstring "'''",
  vstring "''",
  vstring "\n",
  eof
]) <?> "left delimiter"

  
-- Utility

vstring :: String -> Parser String Unit
vstring str = string str *> pure unit


anyString :: Parser String String 
anyString = concat <$> many char


anyStringTill :: Parser String _ -> Parser String String 
anyStringTill p = concat <$> manyTill char p


concat :: forall m. (Monoid m) => [m] -> m
concat elems = impl mempty elems where
	impl acc (x:xs) = impl (acc <> x) xs
	impl acc [    ] = acc


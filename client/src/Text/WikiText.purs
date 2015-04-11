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

import qualified Data.String as String
import qualified Data.Array as Array
import Data.TextFormat
import Data.Monoid
import Data.WikiText
import Data.Maybe
import Data.Either
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
  (try mediaLink <?> "media link") <|> 
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
-- parses media items
--
mediaLink :: WikiTextParser WikiText
mediaLink = do
  string "[["

  mediaType <- mediaTypes <* string ":" 
  mediaLink <- anyStringTill (string "|")
  mediaMeta <- anyStringTillExceptBefore "[[" "]]"

  case splitEnd (String.split "|" mediaMeta) of
    Nothing -> fail ("couldn't split media info for " ++ show mediaMeta)
    Just { init: mediaInfo, last: body } -> do
      mediaText <- withParser bodyParser body
      pure (Media mediaType mediaLink mediaInfo mediaText)

  where

    bodyParser = manyTill bodyText eof 
    mediaTypes = onString "File" File


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
    parser  = divider *> anyStringTill divider
    divider = string sep 


-- PLAIN TEXT


--
-- plain old text
--
anyText :: WikiTextParser BodyText
anyText = PlainText <$> anyStringTill leftDelimiter


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
link = linkImpl "[[[" "]]]" External
   <|> linkImpl "[["  "]]"  Internal where   

  linkImpl left right t = do 
    --
    -- left delimiter && link target
    --
    linkTarget <- string left *> anyStringTill endOfLinkDelimiter
    --
    -- optional display text
    --
    displayText <- optionMaybe do
      string "|" *> anyStringTill (lookAhead (string right))
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
      endOfLinkDelimiter = lookAhead (string "|" <|> string right) 

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
    parser  = divider *> anyStringTill divider
    divider = string sep 


leftDelimiter :: Parser String Unit
leftDelimiter = lookAhead (choice [
  vstring "|",
  vstring "[[[",
  vstring "]]]",
  vstring "[[",
  vstring "]]",
  vstring "'''''",
  vstring "'''",
  vstring "''",
  vstring "\n",
  eof
]) <?> "left delimiter"

  
-- Utility

type EndSplit a b = { init :: [a], last :: b }  


withParser :: forall s a. Parser s a -> s -> Parser s a
withParser parser input = case runParser input parser of
  Left (ParseError error) -> fail error.message
  Right result -> pure result


splitEnd :: forall a. [a] -> Maybe (EndSplit a a)
splitEnd list = do
  init <- Array.init list
  last <- Array.last list
  pure { init: init, last: last }


onString :: forall a. String -> a -> Parser String a
onString str e = string str *> pure e


vstring :: String -> Parser String Unit
vstring str = string str *> pure unit


anyString :: Parser String String 
anyString = concat <$> many char


anyStringTill :: Parser String _ -> Parser String String 
anyStringTill p = concat <$> manyTill char p


--
-- So `anyStringTillExceptBefore "[[" "]]"` will parse this
--
--   blah blah [[hello]] [[hello]] .]]
--   0.........1......0..1......0...Done
--
anyStringTillExceptBefore :: String -> String -> Parser String String 
anyStringTillExceptBefore except end = impl 0 mempty where

  exceptions :: String -> Number
  exceptions s = (Array.length (String.split except s)) - 1

  impl :: Number -> String -> Parser String String
  impl n acc = do
    content <- anyStringTill (string end)
    case exceptions content of 
      count
        | count > 0 -> impl (n + count - 1) (acc ++ content ++ end)
        | n     > 0 -> impl (n - 1) (acc ++ content ++ end)
        | otherwise -> pure (acc ++ content)


concat :: forall m. (Monoid m) => [m] -> m
concat elems = impl mempty elems where
	impl acc (x:xs) = impl (acc <> x) xs
	impl acc [    ] = acc


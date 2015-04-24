module Text.WikiText.Tokens (

  tokens,
  WikiTokenParser(..)

) where

import Control.Alt
import Control.Alternative
import Control.Apply
import Control.Monad.Eff
import Control.Lazy

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token

import Data.WikiText.Tokens
import Data.TextFormat
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Tuple as Tuple 
import qualified Data.Maybe as Maybe 

import Util.Array
import Util.Monoid


type WikiTokenParser a = Parser String a 


tokens :: WikiTokenParser [WikiToken]
tokens = (wikitoken <?> "wikitoken") `manyTill` eof


wikitoken :: WikiTokenParser WikiToken
wikitoken = space
        <|> xml
        <|> pipe
        <|> punctuation
        <|> delimiter
        <|> linebreak 
        <|> ambigious
        <|> word


linebreak :: WikiTokenParser WikiToken
linebreak = Linebreak `onString` "\n" <?> "line break"


space :: WikiTokenParser WikiToken
space = Space `onString` " " <?> "space"


pipe :: WikiTokenParser WikiToken
pipe = Pipe `onString` "|" <?> "pipe"


ambigious :: WikiTokenParser WikiToken
ambigious = assignOperator `onString` "=" where
  assignOperator = Ambigious [
    AmbigiousDelimiter (DeHeading 1), 
    NamedParameterAssignment
  ]


word :: WikiTokenParser WikiToken
word = Word <$> wordString


punctuation :: WikiTokenParser WikiToken
punctuation = try (Punctuation <$> parser) where
  parser = PPeroid `onString` "."  
       <|> PComma `onString` ","
       <|> PExclaim `onString` "!"
       <|> PQuestion `onString` "?"


delimiter :: WikiTokenParser WikiToken
delimiter = (OpeningDelimiter <$> opening)
        <|> (ClosingDelimiter <$> closing) 
        <|> (AmbigiousDelimiter <$> ambigious) where

  opening :: WikiTokenParser Delimiter 
  opening = DeXLink `onString` "[[["
        <|> DeLink `onString` "[["
        <|> DeTempPar `onString` "{{{"
        <|> DeTemp `onString` "{{"


  closing :: WikiTokenParser Delimiter
  closing = DeXLink `onString` "]]]"
        <|> DeLink `onString` "]]"
        <|> DeTempPar `onString` "}}}"
        <|> DeTemp `onString` "}}"


  --
  -- Delimiters which by themselves are immediately obvious
  -- whether they perform a closing opening of a scope
  --
  ambigious :: WikiTokenParser AmbigiousDelimiter 
  ambigious = DeFormat ItalicBold `onString` "'''''"
          <|> DeFormat Bold `onString` "'''"
          <|> DeFormat Italic `onString` "''"
          <|> DeHeading 6 `onString` "======"
          <|> DeHeading 5 `onString` "====="
          <|> DeHeading 4 `onString` "===="
          <|> DeHeading 3 `onString` "==="
          <|> DeHeading 2 `onString` "=="


xml :: WikiTokenParser WikiToken
xml = Xml <$> (closingTag <|> selfClosingTag <|> openingTag) where

  openingTag = try do 
    string "<" *> skipSpaces
    tagName <- wordString <* skipSpaces
    attrs   <- attributes ">"
    pure (Opening tagName attrs)

  closingTag = try do 
    string "</" *> skipSpaces
    tagName <- wordString
    skipSpaces *> string ">"
    pure (Closing tagName)

  selfClosingTag = try do 
    string "<" *> skipSpaces
    tagName <- wordString <* skipSpaces
    attrs   <- attributes "/>"
    pure (SelfClosing tagName attrs)


  attributes end = do
    attrs <- (manyTill attribute (string end))
    pure (Map.fromList attrs)

    where
      attribute = do
        name  <- wordStringImpl ["="] <* skipSpaces <* string "="
        value <- string "\"" *> wordStringImpl ["\""] <* string "\""
        pure (Tuple.Tuple name value) <* skipSpaces

--
-- Utility
--


wordStringImpl :: [String] -> Parser String String 
wordStringImpl other = concat <$> char `manyTill` delimiter where
  delimiter    = lookAhead (specialChars *> pure unit) <|> eof
  specialChars = oneOf (other ++ whitespace ++ special ++ punctuations)


wordString :: Parser String String 
wordString = wordStringImpl []


onString :: forall a. a -> String -> Parser String a
onString a s = string s *> pure a 


-- 
-- COMMON
--


special = ["{", "}", "<", ">", "[", "]", "&", "''", "/", "|"]


punctuations = [".", "!", "?", ","]


whitespace = [" ", "\n"]



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
  assignOperator = Ambigious (Set.fromList [
    ClosingDelimiter (DeHeading 1), 
    NamedParameterAssignment
  ])


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
        <|> DeHeading 6 `onString` "\n======"
        <|> DeHeading 5 `onString` "\n====="
        <|> DeHeading 4 `onString` "\n===="
        <|> DeHeading 3 `onString` "\n==="
        <|> DeHeading 2 `onString` "\n=="
        <|> DeHeading 1 `onString` "\n="

  closing :: WikiTokenParser Delimiter
  closing = DeXLink `onString` "]]]"
        <|> DeLink `onString` "]]"
        <|> DeTempPar `onString` "}}}"
        <|> DeTemp `onString` "}}"
        <|> DeHeading 6 `onString` "======"
        <|> DeHeading 5 `onString` "====="
        <|> DeHeading 4 `onString` "===="
        <|> DeHeading 3 `onString` "==="
        <|> DeHeading 2 `onString` "=="

  --
  -- Delimiters which by themselves are immediately obvious
  -- whether they perform a closing opening of a scope
  --
  ambigious :: WikiTokenParser AmbigiousDelimiter 
  ambigious = DeFormat ItalicBold `onString` "'''''"
          <|> DeFormat Bold `onString` "'''"
          <|> DeFormat Italic `onString` "''"


xml :: WikiTokenParser WikiToken
xml = Xml <$> (closingTag <|> selfClosingTag <|> openingTag) where

  openingTag = try do 
    string "<" *> skipSpaces
    tagName <- wordString
    skipSpaces *> string ">"
    pure (Opening tagName)

  closingTag = try do 
    string "</" *> skipSpaces
    tagName <- wordString
    skipSpaces *> string ">"
    pure (Closing tagName)

  selfClosingTag = try do 
    string "<" *> skipSpaces
    tagName <- wordString
    skipSpaces *> string "/>"
    pure (SelfClosing tagName)

--
-- Utility
--


wordString :: Parser String String 
wordString = concat <$> char `manyTill` delimiter where
  delimiter    = lookAhead (specialChars *> pure unit) <|> eof
  specialChars = oneOf (whitespace ++ special ++ punctuations)


onString :: forall a. a -> String -> Parser String a
onString a s = string s *> pure a 


-- 
-- COMMON
--


special = ["{", "}", "<", ">", "[", "]", "&", "''", "/", "|"]


punctuations = [".", "!", "?", ","]


whitespace = [" ", "\n"]



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

import Util.Array
import Util.Monoid


type WikiTokenParser a = Parser String a 


tokens :: WikiTokenParser [WikiToken]
tokens = (wikitoken <?> "wikitoken") `manyTill` eof


wikitoken :: WikiTokenParser WikiToken
wikitoken = space
        <|> punctuation
        <|> delimiter
        <|> linebreak 
        <|> ambigious
        <|> word


linebreak :: WikiTokenParser WikiToken
linebreak = Linebreak `onString` "\n" <?> "line break"


space :: WikiTokenParser WikiToken
space = Space `onString` " " <?> "space"


ambigious :: WikiTokenParser WikiToken
ambigious = assignOperator `onString` "=" where
  assignOperator = Ambigious [ClosingDelimiter (DeHeading 1), NamedParameterAssignment] 


word :: WikiTokenParser WikiToken
word = Word <$> (concat <$> char `manyTill` delimiter) where
  delimiter    = lookAhead (specialChars *> pure unit) <|> eof
  specialChars = oneOf (whitespace ++ special ++ punctuations) <?> "valid word char"


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





--
-- Utility
--


onString :: forall a. a -> String -> Parser String a
onString a s = string s *> pure a 


-- 
-- COMMON
--


special = ["{", "}", "<", ">", "[", "]", "&", "''"]


punctuations = [".", "!", "?", ","]


whitespace = [" ", "\n"]



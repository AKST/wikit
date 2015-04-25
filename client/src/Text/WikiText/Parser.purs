module Text.WikiText.Parser where

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.Token

import Control.Apply
import Control.Plus
import Control.Alt

import Data.WikiText.Tokens
import Data.WikiText
import qualified Data.Array as Array
import qualified Util.Array as Array
import qualified Data.Maybe as Maybe 


type WikiTextParser a = Parser [WikiToken] a 


wikiText :: WikiTextParser [WikiText]
wikiText = syntax `manyTill` match EndOfInput 


syntax :: WikiTextParser WikiText
syntax = heading
     <|> paragraph
     <|> fail "unsupported token"
 

--
-- A paragraph is made up of more text atom. 
--
paragraph :: WikiTextParser WikiText
paragraph = Paragraph <$> anyText `many1Till` delimiter where
  delimiter = match Linebreak <|> lookAhead (match EndOfInput)
 

heading :: WikiTextParser WikiText
heading = try (nextType LinebreakType) *> do
  size <- getHeadingDelimiter  
  text <- textTillHeadingOf size 
  pure (Heading size text) where

    getHeadingDelimiter = ambigiousDelimiter HeadingType >>= getSize where 
      getSize (DeHeading size) = pure size
      getSize _                = fail "getHeadingDelimiter: not a heading" 

    textTillHeadingOf size = anyText `manyTill` delimiter where
      delimiter = match (AmbigiousDelimiter (DeHeading size)) 


--
-- atoms
--

anyText :: WikiTextParser WikiAtom
anyText = choices <?> "any text" where 
  choices = word


word :: WikiTextParser WikiAtom
word = (tokenToSyntax <$> skipSpace (try (nextType WordType))) <?> "word" where
  tokenToSyntax (Word text) = (WordAtom text)


-- 
-- -- predicates
-- 
 

data AmbigiousDelimiterType
  = HeadingType
  | FormatType


instance eqAmbigiousDelimiterType :: Eq AmbigiousDelimiterType where
  (/=) l r = not (l == r)
  (==) HeadingType HeadingType = true
  (==) FormatType FormatType = true
  (==) _ _ = false


ambigiousDelimiter :: AmbigiousDelimiterType -> WikiTextParser AmbigiousDelimiter
ambigiousDelimiter tokenType = nextType ADelimiterType >>= impl where

  impl (AmbigiousDelimiter token@(DeHeading _)) | tokenType == HeadingType = pure token
  impl (AmbigiousDelimiter token@(DeFormat _))  | tokenType == FormatType = pure token
  impl e = fail ("somehow " ++ show e ++ " was passed to ambigiousDelimiter")

 
-- delimiterOpening :: Delimiter -> DocParser WikiToken
-- delimiterOpening delimterType = nextType ODelimiterType >>= \delimter ->
--   case delimter of
--     OpeningDelimiter otherType | otherType == delimterType -> pure delimter
--     _                                                      -> empty


-- 
-- -- methods
--   
 
data TokenType
  = ODelimiterType 
  | ADelimiterType
  | WordType
  | SpaceType
  | LinebreakType
  | EndOfInputType


instance eqTokenType :: Eq TokenType where
  (/=) l r = not (l == r)

  (==) ODelimiterType ODelimiterType = true
  (==) ADelimiterType ADelimiterType = true
  (==) WordType WordType = true
  (==) SpaceType SpaceType = true
  (==) LinebreakType LinebreakType = true
  (==) EndOfInputType EndOfInputType = true
  (==) _ _ = false


--
-- a more general match so we can match on the type of token
-- as opposed to the exact value of the token.
--
nextType :: TokenType -> WikiTextParser WikiToken
nextType tokenType = token >>= matchType where 

  matchType token@(Word _)               | tokenType == WordType = pure token
  matchType token@(Space)                | tokenType == SpaceType = pure token
  matchType token@(AmbigiousDelimiter _) | tokenType == ADelimiterType = pure token
  matchType token@(OpeningDelimiter _)   | tokenType == ODelimiterType = pure token
  matchType token@(EndOfInput)           | tokenType == EndOfInputType = pure token
  matchType token@(Linebreak)            | tokenType == LinebreakType = pure token 
  matchType (Ambigious choices) = firstMatch choices where 
    firstMatch (x:xs) = matchType x <|> firstMatch xs 
    firstMatch []     = empty  
  matchType _ = empty

 
skipSpace :: forall a. WikiTextParser a -> WikiTextParser a
skipSpace parser = skipMany (match Space) *> parser 


-- 
-- -- low level methods
-- 
-- 
-- takeWhile :: (WikiToken -> Boolean) -> DocParser [WikiToken] 
-- takeWhile predicate = lift State.get >>= impl where
--   impl state = withTokens [] state.tokens where
--     withTokens acc tokens = do
--       split <- nothingError (Intern RanOutOfTokens) (Array.splitStart tokens)
--       if predicate split.head
--         then withTokens (acc ++ [split.head]) split.tail 
--         else do
--           State.put (state { tokens = tokens })
--           pure acc
-- 
-- 
-- popToken :: DocParser WikiToken
-- popToken = do
--   state <- lift State.get  
--   split <- nothingError (Intern RanOutOfTokens) (Array.splitStart state.tokens)
--   State.put (state { tokens = split.tail })
--   pure split.head
-- 
-- 
-- -- util 
-- 
-- 
-- nothingError :: forall a e m. (Applicative m, Error.MonadError e m) 
--              => e -> Maybe.Maybe a -> m a  
-- 
-- nothingError error maybe = 
--   case maybe of
--     Maybe.Nothing -> Error.throwError error 
--     Maybe.Just vl -> pure vl



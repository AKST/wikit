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
import qualified Data.Either as Either
import qualified Data.Array as Array
import qualified Util.Array as Array
import qualified Data.Maybe as Maybe 


type WikiTextParser a = Parser [WikiToken] a 


wikiText :: WikiTextParser [WikiText]
wikiText = pure [] 


-- syntax :: WikiTextParser WikiText
-- syntax = paragraph
-- 
-- 
-- paragraph :: WikiTextParser WikiText
-- paragraph = Paragraph <$> anyText `manyTill` match EndOfInput 
 

-- heading = do
--   size <- getHeadingDelimiter  
--   text <- textTillHeadingEndOf size 
--   pure (Heading size text) where
-- 
--     getHeadingDelimiter = ambigiousDelimiter HeadingType >>= 
--       \delimiter -> case delimiter of 
--         AmbigiousDelimiter (Heading size) -> pure size
--         _                                 -> empty 
-- 
--     textTillHeadingEndOf size = wikiText `endBy` (match (DeHeading size)) 


--
-- atoms
--

anyText :: WikiTextParser WikiAtom
anyText = word


word :: WikiTextParser WikiAtom
word = tokenToSyntax <$> skipSpace (nextIs WordType) where
  tokenToSyntax (Word text) = (WordAtom text)

-- 
-- -- 
-- -- -- predicates
-- -- 
-- 
-- data AmbigiousDelimiterType
--   = HeadingType
--   | FormatType
-- 
-- 
-- ambigiousDelimiter :: AmbigiousDelimiterType -> DocParser WikiToken
-- ambigiousDelimiter tokenType = nextIs ADelimiterType >>= impl tokenType where
--   impl HeadingType token@(AmbigiousDelimiter (DeHeading _)) = pure token
--   impl FormatType  token@(AmbigiousDelimiter (DeFormat _))  = pure token
--   impl _           _                                        = empty

-- 
-- delimiterOpening :: Delimiter -> DocParser WikiToken
-- delimiterOpening delimterType = nextIs ODelimiterType >>= \delimter ->
--   case delimter of
--     OpeningDelimiter otherType | otherType == delimterType -> pure delimter
--     _                                                      -> empty
-- 
-- 
-- -- methods
--   
 
data TokenType
  = ODelimiterType 
  | ADelimiterType
  | WordType
  | SpaceType


nextIs :: TokenType -> WikiTextParser WikiToken
nextIs tokenType = token >>= matchType tokenType where 
  matchType WordType token@(Word _) = pure token
  matchType SpaceType token@(Space) = pure token
  matchType ODelimiterType token@(OpeningDelimiter _)   = pure token
  matchType ADelimiterType token@(AmbigiousDelimiter _) = pure token
  matchType _ (Ambigious choices) = firstMatch choices where 
    firstMatch (x:xs) = matchType tokenType x <|> firstMatch xs 
    firstMatch []     = empty  
  matchType _ _ = empty

 
skipSpace :: forall a. WikiTextParser a -> WikiTextParser a
skipSpace parser = skipMany (nextIs SpaceType) *> parser 


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



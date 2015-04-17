module Data.WikiText.Tokens (

  Xml(..), 
  WikiToken(..),
  Punctuation(..),
  Delimiter(..),
  AmbigiousDelimiter(..)

) where

import Data.TextFormat
import qualified Data.Set as Set
import qualified Data.Map as Map


data Xml
  = Opening String (Map.Map String String)
  | Closing String
  | SelfClosing String (Map.Map String String)


data WikiToken
  = Linebreak
  | Space
  | Pipe
  | Xml Xml
  | Word String
  | Punctuation Punctuation
  | OpeningDelimiter Delimiter
  | ClosingDelimiter Delimiter
  | AmbigiousDelimiter AmbigiousDelimiter
  | NamedParameterAssignment
  | Ambigious (Set.Set WikiToken)


data Punctuation 
  = PPeroid
  | PExclaim
  | PQuestion
  | PComma


data Delimiter
  = DeLink
  | DeXLink
  | DeTemp
  | DeTempPar
  | DeHeading Number


data AmbigiousDelimiter
  = DeFormat TextFormat


--
-- Eq
--


instance eqWikiToken :: Eq WikiToken where
  (/=) a b = not (a == b)

  (==) (Punctuation p1) (Punctuation p2) = p2 == p2
  (==) (AmbigiousDelimiter a) (AmbigiousDelimiter b) = a == b
  (==) (ClosingDelimiter a) (ClosingDelimiter b) = a == b
  (==) (OpeningDelimiter a) (OpeningDelimiter b) = a == b
  (==) (Word w1) (Word w2) = w1 == w2
  (==) Linebreak Linebreak = true
  (==) Space Space = true
  (==) NamedParameterAssignment NamedParameterAssignment = true
  (==) (Ambigious a) (Ambigious b) = a == b
  (==) (Xml a) (Xml b) = a == b
  (==) _ _ = false
  

instance eqPunctuation :: Eq Punctuation where
  (/=) a b = not (a == b)

  (==) PPeroid PPeroid = true
  (==) PExclaim PExclaim = true
  (==) PComma PComma = true
  (==) PQuestion PQuestion = true
  (==) _ _ = false


instance eqDelimiter :: Eq Delimiter where
  (/=) a b = not (a == b)
  
  (==) DeLink DeLink = true
  (==) DeXLink DeXLink = true
  (==) DeTemp DeTemp = true
  (==) DeTempPar DeTempPar = true
  (==) (DeHeading a) (DeHeading b) = a == b
  (==) _ _ = false


instance eqAmbigiousDelimiter :: Eq AmbigiousDelimiter where
  (/=) a b = not (a == b)
  
  (==) (DeFormat a) (DeFormat b) = a == b
  (==) _ _ = false


instance eqXml :: Eq Xml where
  (/=) a b = not (a == b)

  (==) (Opening a ma) (Opening b mb) = a == b && ma == mb
  (==) (Closing a) (Closing b) = a == b
  (==) (SelfClosing a ma) (SelfClosing b mb) = a == b && ma == mb
  (==) _ _ = false



--
-- Show
--


instance showWikiToken :: Show WikiToken where
  show (Word w) = "Word " ++ show w
  show Linebreak = "Linebreak"
  show (Punctuation p) = "Punctuation " ++ show p
  show (OpeningDelimiter d) = "OpeningDelimiter (" ++ show d ++ ")"
  show (ClosingDelimiter d) = "ClosingDelimiter (" ++ show d ++ ")"
  show (AmbigiousDelimiter d) = "AmbigiousDelimiter (" ++ show d ++ ")"
  show NamedParameterAssignment = "NamedParameterAssignment"
  show (Ambigious as) = "Ambigious " ++ show as 
  show (Xml t) = "Xml (" ++ show t ++ ")" 
  show Space = "Space"


instance showDelimiter :: Show Delimiter where
  show DeLink = "DeLink"
  show DeXLink = "DeXLink"
  show DeTemp = "DeTemp"
  show DeTempPar = "DeTempPar"
  show DeLink = "DeLink"
  show (DeHeading h) = "DeHeading (" ++ show h ++ ")"


instance showAmbigiousDelimiter :: Show AmbigiousDelimiter where
  show (DeFormat f) = "DeFormat (" ++ show f ++ ")"


instance showPunctuation :: Show Punctuation where
  show PPeroid = "PPeroid" 
  show PExclaim = "PExclaim"
  show PComma = "PComma"
  show PQuestion = "PQuestion"


instance showXml :: Show Xml where
  show (Closing n) = "Closing " ++ show n
  show (Opening n m) = "Opening " ++ show n ++ " " ++ show m
  show (SelfClosing n m) = "SelfClosing " ++ show n ++ " " ++ show m


--
-- -- todo newtype wrap this
--
instance ordWikiText :: Ord WikiToken where
  compare Linebreak other = case other of
    Linebreak -> EQ
    _ -> GT
  compare Space other = case other of
    Linebreak -> LT
    Space -> EQ
    _ -> GT
  compare Pipe other = case other of
    Linebreak -> LT
    Space -> LT
    Pipe -> EQ
    _ -> GT
  compare (Xml xml) other = case other of
    Linebreak -> LT
    Space -> LT
    Pipe -> LT
    Xml otherXml -> case [xml, otherXml] of
      --
      -- todo compare attributes
      --
      [Opening a _, Opening b _] -> a `compare` b
      [Opening _ _, _        ] -> GT
      [Closing a, Opening b _] -> LT
      [Closing a, Closing b] -> a `compare` b
      [Closing _, _        ] -> GT
      [SelfClosing a _, SelfClosing b _] -> a `compare` b
      [SelfClosing a _, _              ] -> LT
    _ -> GT
  compare (Word word) other = case other of
    Linebreak -> LT
    Space -> LT
    Pipe -> LT
    Xml _ -> LT
    Word otherWord -> word `compare` otherWord
    _ -> GT
  compare (Punctuation p) other = case other of
    Linebreak -> LT
    Space -> LT
    Pipe -> LT
    Xml _ -> LT 
    Word _ -> LT
    Punctuation otherP -> case [p, otherP] of
      [PPeroid, PPeroid] -> EQ
      [PPeroid, _      ] -> GT
      [PExclaim, PPeroid] -> LT
      [PExclaim, PExclaim] -> EQ
      [PExclaim, _       ] -> GT
      [PQuestion, PPeroid] -> LT
      [PQuestion, PExclaim] -> LT
      [PQuestion, PQuestion] -> EQ
      [PQuestion, _        ] -> GT 
      [PComma, PComma] -> EQ 
      [PComma, _     ] -> LT
    _ -> GT
  compare (OpeningDelimiter d) other = case other of
    Ambigious _ -> GT
    NamedParameterAssignment -> GT 
    ClosingDelimiter _ -> GT
    AmbigiousDelimiter _ -> GT
    OpeningDelimiter otherD -> compareDelimiter d otherD
    _ -> LT
  compare (AmbigiousDelimiter d) other = case other of
    Ambigious _ -> GT
    NamedParameterAssignment -> GT 
    ClosingDelimiter _ -> GT
    AmbigiousDelimiter otherD -> case [d, otherD] of
      [DeFormat Italic, DeFormat Italic] -> EQ
      [DeFormat Italic, DeFormat _     ] -> GT
      [DeFormat Bold, DeFormat Italic] -> LT
      [DeFormat Bold, DeFormat Bold] -> EQ
      [DeFormat Bold, DeFormat _] -> GT
      [DeFormat Bold, DeFormat _] -> GT
      [DeFormat ItalicBold, DeFormat ItalicBold] -> EQ
      [DeFormat ItalicBold, DeFormat _         ] -> LT
    _ -> LT
  compare (ClosingDelimiter d) other = case other of
    Ambigious _ -> GT
    NamedParameterAssignment -> GT 
    ClosingDelimiter otherD -> d `compareDelimiter` otherD
  compare NamedParameterAssignment other = case other of
    Ambigious _ -> GT 
    NamedParameterAssignment -> EQ
    _ -> LT
  --
  -- TODO reimplement once a instance is available
  --
  -- compare (Ambigious a) other = case other of
  --   Ambigious otherA -> a `compare` otherA
  --   _ -> LT

compareDelimiter :: Delimiter -> Delimiter -> Ordering
compareDelimiter a b = case a of
  DeLink -> case b of
    DeLink -> EQ
    _      -> GT
  DeXLink -> case b of
    DeLink -> LT
    DeXLink -> EQ
    _      -> GT
  DeTemp -> case b of
    DeLink -> LT
    DeXLink -> LT
    DeTemp -> EQ
    _ -> GT
  DeTempPar -> case b of
    DeHeading _ -> GT
    DeTempPar -> EQ
    _         -> LT
  DeHeading ah -> case b of
    DeHeading bh -> ah `compare` bh
    _            -> LT




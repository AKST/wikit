module Data.WikiText.Tokens where

import Data.TextFormat


data Xml
  = Opening String
  | Closing String
  | SelfClosing String



data WikiToken
  = Linebreak
  | Space
  | Xml Xml
  | Word String
  | Punctuation Punctuation
  | OpeningDelimiter Delimiter
  | ClosingDelimiter Delimiter
  | AmbigiousDelimiter AmbigiousDelimiter
  | NamedParameterAssignment
  | Ambigious [WikiToken]


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
  | DeXml String 
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
  (==) (DeXml a) (DeXml b) = a == b 
  (==) (DeHeading a) (DeHeading b) = a == b
  (==) _ _ = false


instance eqAmbigiousDelimiter :: Eq AmbigiousDelimiter where
  (/=) a b = not (a == b)
  
  (==) (DeFormat a) (DeFormat b) = a == b
  (==) _ _ = false


instance eqXml :: Eq Xml where
  (/=) a b = not (a == b)

  (==) (Opening a) (Opening b) = a == b
  (==) (Closing a) (Closing b) = a == b
  (==) (SelfClosing a) (SelfClosing b) = a == b
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
  show (DeXml t) = "DeXml (" ++ show t ++ ")"
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
  show (Opening n) = "Opening " ++ show n
  show (SelfClosing n) = "SelfClosing " ++ show n



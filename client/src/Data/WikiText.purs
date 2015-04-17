module Data.WikiText where

import Data.TextFormat
import Data.Maybe


data WikiText
  = Text BodyText
  | LineBreak
  | Heading Number String
  | Media MediaType String [String] [BodyText]

  -- --
  -- -- {{ curly syntax }}
  -- --
  -- | Variable { name :: String }
  | Template String [TemplateArg]
  -- | Parameter WikiParameter

  -- --
  -- -- XML / HTML
  -- --
  -- | Extension { tag :: String, body :: WikiText }
  -- | Comment { body :: String }
  -- | NoWiki { body :: String } 


data BodyText
  = PlainText String
  | FormatText TextFormat String
  -- 
  -- 2nd parameter link target
  -- 3rd parameter link text
  --
  | Link LinkType String (Maybe String)


data LinkType = External | Internal


data MediaType 
  = File


data TemplateArg 
  = PlainArg [BodyText]
  | NamedArg String [BodyText] 
  

data WikiParameter
  = TextParameter { name :: String, text :: String }
  | RefParameter { name :: String, param :: String }


-- EQ INSTANCES 


instance eqWikiText :: Eq WikiText where
  (/=) l r = not (l == r)
  
  (==) LineBreak LineBreak = true
  (==) (Text t1) (Text t2) = t1 == t2
  (==) (Heading s1 b1) (Heading s2 b2) = s1 == s2 && b1 == b2
  (==) (Media mt1 u1 i1 t1) (Media mt2 u2 i2 t2) = mt1 == mt1 && u1 == u2 && t1 == t2 && i1 == i2
  (==) (Template n1 a1) (Template n2 a2) = n1 == n2 && a1 == a2
  -- (==) (Variable { name: n1 }) (Variable { name: n2 }) = n1 == n2
  -- (==) (Parameter param1)      (Parameter param2)      = param1 == param2
  -- (==) (Comment { body: b1 })  (Comment { body: b2 })  = b1 == b2
  -- (==) (NoWiki { body: b1 })   (NoWiki { body: b2 })   = b1 == b2
  -- (==) (Extension { tag: t1, body: b1 }) (Extension { tag: t2, body: b2 }) = t1 == t2 && b1 == b2
  (==) _ _ = false
  
instance eqBodyText :: Eq BodyText where
  (/=) l r = not (l == r)

  (==) (PlainText b1) (PlainText b2) = b1 == b2
  (==) (FormatText f1 b1) (FormatText f2 b2) = f1 == f2 && b1 == b2
  (==) (Link t1 l1 text1) (Link t2 l2 text2) = t1 == t2 && l1 == l2 && text1 == text2 
  (==) _ _ = false

instance eqLinkType :: Eq LinkType where
  (/=) l r = not (l == r)
  (==) Internal Internal = true
  (==) External External = true 
  (==) _ _ = false


instance eqWikiTemplateArg :: Eq TemplateArg where 
  (/=) l r = not (l == r)

  (==) (PlainArg n1) (PlainArg n2)       = n1 == n2
  (==) (NamedArg n1 p1) (NamedArg n2 p2) = n1 == n2 && p1 == p2

  (==) _ _ = false

instance eqWikiParameter :: Eq WikiParameter where
  (/=) l r = not (l == r)

  (==) (TextParameter { name: n1, text: t1 }) (TextParameter { name: n2, text: t2 }) = 
    n1 == n2 && t1 == t2
  (==) (RefParameter { name: n1, param: p1 }) (RefParameter { name: n2, param: p2 }) = 
    n1 == n2 && p1 == p2

  (==) _ _ = false

instance eqMediaType :: Eq MediaType where 
  (/=) l r = not (l == r)

  (==) File File = true
  (==) _ _ = false



-- SHOW INSTANCES


instance showWikiText :: Show WikiText where
  show (Text b) = "Text (" ++ show b ++ ")"
  show (Heading s b) = "Heading " ++ show s ++ " " ++ show b
  show LineBreak = "LineBreak"
  show (Media mt u i t) = "Media " 
    ++ show mt ++ " " ++ show u ++ " " 
    ++ show i ++ " " ++ show t
  show (Template n args) = "Template " ++ show n ++ " " ++ show args

  -- show (Variable { name: name })          = "Variable { name: " ++ show name ++ " }"
  -- show (Template template)                = "Template (" ++ show template ++ ")"
  -- show (Parameter param)                  = "Parameter (" ++ show param ++ ")"
  -- show (Comment { body: body })           = "Comment { body: " ++ show body ++ " })"
  -- show (NoWiki { body: body })            = "NoWiki { body: " ++ show body ++ " })"

  -- show (Extension { tag: t, body: body }) = "Extension { tag: " ++ show t ++ ", body: " ++ show body ++ " })"

instance showBodyText :: Show BodyText where
  show (PlainText body) = "PlainText " ++ show body
  show (FormatText f b) = "FormatText " ++ show f ++ " " ++ show b
  show (Link t l text) = "Link " ++ show t ++ " " ++ show l ++ " " ++ show text

instance showLinkType :: Show LinkType where
  show External = "External"
  show Internal = "Internal"

instance showWikiTemplateArg :: Show TemplateArg where 
  show (PlainArg n) = "PlainArg " ++ show n
  show (NamedArg n p) = "NamedArg " ++ show n ++ " " ++ show p

instance showWikiParameter :: Show WikiParameter where
  show (TextParameter { name: n, text: t }) = "TextParameter { name: " ++ show n ++ ", text: " ++ show t ++ "}"
  show (RefParameter { name: n, param: p }) = "RefParameter { name: " ++ show n ++ ", param: " ++ show p ++ "}"

instance showMediaType :: Show MediaType where
  show File = "File"



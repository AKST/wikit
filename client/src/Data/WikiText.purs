module Data.WikiText where


data WikiText
  --
  -- {{ curly syntax }}
  --
  = Variable { name :: String }
  | Template WikiTemplate
  | Parameter WikiParameter

  --
  -- XML / HTML
  --
  | Extension { tag :: String, body :: WikiText }
  | Comment { body :: String }
  | NoWiki { body :: String } 

  | AnyText { body :: String }


data WikiTemplate 
  = NormalTemplate { name :: String, text :: String }
  | ParamTemplate { name :: String, param :: String, fallback :: String }
  

data WikiParameter
  = TextParameter { name :: String, text :: String }
  | RefParameter { name :: String, param :: String }

-- EQ INSTANCES 

instance eqWikiText :: Eq WikiText where
  (/=) l r = not (l == r)
  
  (==) (Variable { name: n1 }) (Variable { name: n2 }) = n1 == n2
  (==) (Template template1)    (Template template2)    = template1 == template2
  (==) (Parameter param1)      (Parameter param2)      = param1 == param2
  (==) (Comment { body: b1 })  (Comment { body: b2 })  = b1 == b2
  (==) (NoWiki { body: b1 })   (NoWiki { body: b2 })   = b1 == b2
  (==) (AnyText { body: b1 })  (AnyText { body: b2 })  = b1 == b2

  (==) (Extension { tag: t1, body: b1 }) (Extension { tag: t2, body: b2 }) = t1 == t2 && b1 == b2

  (==) _ _ = false

instance eqWikiTemplate :: Eq WikiTemplate where 
  (/=) l r = not (l == r)

  (==) (NormalTemplate { name: n1, text: t1 }) (NormalTemplate { name: n2, text: t2 }) =
    n1 == n2 && t1 == t2
  (==) (ParamTemplate { name: n1, param: p1 }) (ParamTemplate { name: n2, param: p2 }) =
    n1 == n2 && p1 == p2

  (==) _ _ = false

instance eqWikiParameter :: Eq WikiParameter where
  (/=) l r = not (l == r)

  (==) (TextParameter { name: n1, text: t1 }) (TextParameter { name: n2, text: t2 }) = 
    n1 == n2 && t1 == t2
  (==) (RefParameter { name: n1, param: p1 }) (RefParameter { name: n2, param: p2 }) = 
    n1 == n2 && p1 == p2

  (==) _ _ = false

-- SHOW INSTANCES

instance showWikiText :: Show WikiText where
  show (Variable { name: name })          = "Variable { name: " ++ show name ++ " }"
  show (Template template)                = "Template (" ++ show template ++ ")"
  show (Parameter param)                  = "Parameter (" ++ show param ++ ")"
  show (Comment { body: body })           = "Comment { body: " ++ show body ++ " })"
  show (NoWiki { body: body })            = "NoWiki { body: " ++ show body ++ " })"
  show (AnyText { body: body })           = "AnyText { body: " ++ show body ++ " })"
  show (Extension { tag: t, body: body }) = "Extension { tag: " ++ show t ++ ", body: " ++ show body ++ " })"

instance showWikiTemplate :: Show WikiTemplate where 
  show (NormalTemplate { name: n, text: t }) = "NormalTemplate { name: " ++ show n ++ ", text: " ++ show t ++ "})"
  show (ParamTemplate { name: n, param: p }) = "ParamTemplate { name: " ++ show n ++ ", param: " ++ show p ++ "})"

instance showWikiParameter :: Show WikiParameter where
  show (TextParameter { name: n, text: t }) = "TextParameter { name: " ++ show n ++ ", text: " ++ show t ++ "}"
  show (RefParameter { name: n, param: p }) = "RefParameter { name: " ++ show n ++ ", param: " ++ show p ++ "}"




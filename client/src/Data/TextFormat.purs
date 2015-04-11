module Data.TextFormat where

data TextFormat
  = Italic
  | Bold
  | ItalicBold

instance eqTextFormat :: Eq TextFormat where
  (/=) l r = not (l == r)

  (==) Italic Italic = true
  (==) Bold Bold = true
  (==) ItalicBold ItalicBold = true

  (==) _ _ = false

instance showTextFormat :: Show TextFormat where
  show Italic = "Italic"
  show Bold = "Bold"
  show ItalicBold = "ItalicBold"

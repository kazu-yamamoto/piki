module Types where

import qualified Data.Text.Lazy as L

data Element = HR
             | H Int XText
             | P XText
             | PRE XText
             | UOL Xlist
             | DL [Def]
             | IMG [Image]
             | TABLE [[XText]]
             | DIV DivAttr [Element]
             deriving (Eq,Show)

type URL = L.Text
type Title = L.Text

data PText = Null | R Char | E Char | L [L.Text] | A Title URL deriving (Eq,Show)
type XText = [PText]

data Xlist   = Ulist [Xitem] | Olist [Xitem] | Nil deriving (Eq,Show)
data Xitem   = Item XText Xlist deriving (Eq,Show)
data Def     = Def XText XText deriving (Eq,Show)
data Image   = Image Title URL (Maybe URL) deriving (Eq,Show)
data DivAttr = Class L.Text | Id L.Text deriving (Eq,Show)

getTitle :: [Element] -> XText
getTitle elems = case filter isH elems of
    []  -> []
    h:_ -> fromH h

isH :: Element -> Bool
isH (H _ _) = True
isH _       = False

fromH :: Element -> XText
fromH (H _ title) = title
fromH _           = []

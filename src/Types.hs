module Types where

data Element = HR
             | H Int XString
             | P XString
             | PRE XString
             | UOL Xlist
             | DL [Def]
             | IMG [Image]
             | TABLE [[XString]]
             | DIV DivAttr [Element]
             deriving (Eq,Show)

type URL = String
type Title = String

data PString = Null | R Char | E Char | L String | A Title URL deriving (Eq,Show)
type XString = [PString]

data Xlist   = Ulist [Xitem] | Olist [Xitem] | Nil deriving (Eq,Show)
data Xitem   = Item XString Xlist deriving (Eq,Show)
data Def     = Def XString XString deriving (Eq,Show)
data Image   = Image Title URL (Maybe URL) deriving (Eq,Show)
data DivAttr = Class String | Id String deriving (Eq,Show)

getTitle :: [Element] -> XString
getTitle elems = case filter isH elems of
    []  -> []
    h:_ -> fromH h

isH :: Element -> Bool
isH (H _ _) = True
isH _       = False

fromH :: Element -> XString
fromH (H _ title) = title
fromH _           = []

module Types where

data Element = HR
             | H Int String
             | P String
             | PRE String
             | UOL Xlist
             | DL [Def]
             | IMG [Image]
             | DIV DivAttr [Element]

data Xlist   = Ulist [Xitem] | Olist [Xitem] | Nil
data Xitem   = Item String Xlist
data Def     = Def String String
data Image   = Image String String
data DivAttr = Class String | Id String

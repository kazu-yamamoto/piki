module Html where

import Tag

data Element = HR |
               H Int String |
               P String |
               PRE String |
               UOL Xlist |
               DL [Def] |
               IMG [Image] |
               DIV DivAttr [Element]

data Xlist = Ulist [Xitem] | Olist [Xitem] | Nil
data Xitem = Item String Xlist
data Def   = Def String String
data Image = Image String String
data DivAttr = Class String | Id String

instance Show Element where
    show HR        = solo ("hr",[])
    show (H n str) = str // ("h" ++ show lvl) where lvl = min 6 n
    show (P str)   = str // "p"
    show (PRE str) = str \\ "pre"
    show (UOL xl)  = show xl
    show (DL ds)   = concatMap show ds \\ "dl"
    show (IMG is)  = concatMap show is
    show (DIV (Class val) els) = concatMap show els \\\ ("div",[("class",val)])
    show (DIV (Id val) els)    = concatMap show els \\\ ("div",[("id",val)])

instance Show Def where
    show (Def title desc) = title // "dt" ++ desc // "dd"

instance Show Image where
    show (Image title src) = solo ("img",[("src",src),("alt",title),("title",title)])

instance Show Xlist where
    show (Ulist xls) = concatMap show xls \\ "ul"
    show (Olist xls) = concatMap show xls \\ "ol"
    show Nil         = ""

instance Show Xitem where
    show (Item str Nil) = str // "li"
    show (Item str xls) = (str ++ "\n" ++ (indent $ show xls)) // "li"
      where
        indent = unlines . map idn . lines
        idn xs@('<':_)  = '\t':xs
        idn xs@('\t':_) = '\t':xs
        idn xs          = xs

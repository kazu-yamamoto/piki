module Html where

import Tag
import Types

class ToHtml a where
    toHtml :: a -> String

instance ToHtml Element where
    toHtml HR        = solo ("hr",[])
    toHtml (H n str) = str // ('h' : show lvl) where lvl = min 6 n
    toHtml (P str)   = str // "p"
    toHtml (PRE str) = str \\ "pre"
    toHtml (UOL xl)  = toHtml xl
    toHtml (DL ds)   = concatMap toHtml ds \\ "dl"
    toHtml (IMG is)  = concatMap toHtml is
    toHtml (DIV (Class val) els) = concatMap toHtml els \\\ ("div",[("class",val)])
    toHtml (DIV (Id val) els)    = concatMap toHtml els \\\ ("div",[("id",val)])

instance ToHtml Def where
    toHtml (Def title desc) = title // "dt" ++ desc // "dd"

instance ToHtml Image where
    toHtml (Image title src) = solo ("img",[("src",src),("alt",title),("title",title)])

instance ToHtml Xlist where
    toHtml (Ulist xls) = concatMap toHtml xls \\ "ul"
    toHtml (Olist xls) = concatMap toHtml xls \\ "ol"
    toHtml Nil         = ""

instance ToHtml Xitem where
    toHtml (Item str Nil) = str // "li"
    toHtml (Item str xls) = (str ++ "\n" ++ indent (toHtml xls)) // "li"
      where
        indent = unlines . map idn . lines
        idn xs@('<':_)  = '\t':xs
        idn xs@('\t':_) = '\t':xs
        idn xs          = xs

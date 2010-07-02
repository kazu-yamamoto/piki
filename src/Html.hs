module Html (toHTML) where

import Data.Char
import Data.Maybe
import Piki
import Tag
import Types

----------------------------------------------------------------

toHTML :: String -> String -> String
toHTML tmp inp = replace tmp [("body",body),("title",fromXString title)]
  where
    elems = piki inp
    body = concatMap toHtml elems
    title = getTitle elems

replace :: String -> [(String, String)] -> String
replace tmpl params =
        case break (== '$') tmpl of
          (s, '$':cs) -> s ++ replaced ++ replace (chop rest) params
              where
                (var, rest) = span isAlpha cs
                replaced = fromMaybe ('$':var) (lookup var params)
                chop ('\n':xs) = xs
                chop xs        = xs
          (s, _)     -> s

----------------------------------------------------------------

class ToHtml a where
    toHtml :: a -> String

instance ToHtml Element where
    toHtml HR        = solo ("hr",[])
    toHtml (H n str) = fromXString str // ('h' : show lvl) where lvl = min 6 n
    toHtml (P str)   = fromXString str // "p"
    toHtml (PRE str) = fromXString str \\ "pre"
    toHtml (UOL xl)  = toHtml xl
    toHtml (DL ds)   = concatMap toHtml ds \\ "dl"
    toHtml (IMG is)  = concatMap toHtml is
    toHtml (DIV (Class val) els) = concatMap toHtml els \\\ ("div",[("class",val)])
    toHtml (DIV (Id val) els)    = concatMap toHtml els \\\ ("div",[("id",val)])

instance ToHtml Def where
    toHtml (Def title desc) = fromXString title // "dt" ++ fromXString desc // "dd"

instance ToHtml Image where
    toHtml (Image title src) = solo ("img",[("src",src),("alt",title),("title",title)])

instance ToHtml Xlist where
    toHtml (Ulist xls) = concatMap toHtml xls \\ "ul"
    toHtml (Olist xls) = concatMap toHtml xls \\ "ol"
    toHtml Nil         = ""

instance ToHtml Xitem where
    toHtml (Item str Nil) = fromXString str // "li"
    toHtml (Item str xls) = (fromXString str ++ "\n" ++ indent (toHtml xls)) // "li"
      where
        indent = unlines . map idn . lines
        idn xs@('<':_)  = '\t':xs
        idn xs@('\t':_) = '\t':xs
        idn xs          = xs

----------------------------------------------------------------

fromXString :: [PString] -> String
fromXString = concatMap toStr
  where
    toStr Null          = ""
    toStr (R c)         = referenceChar c
    toStr (E c)         = [c]
    toStr (L xs)        = reference xs
    toStr (A title url) = reference title %%% ("a",[("href",url)])


reference :: String -> String
reference = concatMap referenceChar

referenceChar :: Char -> String
referenceChar '<' = "&lt;"
referenceChar '>' = "&gt;"
referenceChar '&' = "&amp;"
referenceChar '"' = "&quot;"
referenceChar c = [c]

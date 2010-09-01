module Markdown (toMD) where

import Piki
import Types
import Html ((\\\))

----------------------------------------------------------------

toMD :: String -> String
toMD = concatMap toMarkdown . piki

----------------------------------------------------------------

class ToMD a where
    toMarkdown :: a -> String

instance ToMD Element where
    toMarkdown HR        = "********************************\n"
    toMarkdown (H n str) = replicate n '#' ++ " " ++ fromXString str ++ "\n"
    toMarkdown (P str)   = fromXString str ++ "\n\n"
    toMarkdown (PRE str) = unlines . map ("    " ++) . lines . fromXString $ str
    toMarkdown (UOL xl)  = toMarkdown xl
    toMarkdown (DL ds)   = concatMap toMarkdown ds ++ "\n"
    toMarkdown (IMG is)  = concatMap toMarkdown is ++ "\n"
    toMarkdown (DIV (Class val) els) = "\n" ++ concatMap toMarkdown els \\\ ("div",[("class",val)]) ++ "\n"
    toMarkdown (DIV (Id val) els)    = "\n" ++ concatMap toMarkdown els \\\ ("div",[("id",val)]) ++ "\n"

instance ToMD Def where
    toMarkdown (Def title desc) = "* " ++ fromXString title ++ "\n> " ++ fromXString desc ++ "\n"

instance ToMD Image where
    toMarkdown (Image title src) = "![" ++ title ++ "](" ++ src ++ ")\n"

instance ToMD Xlist where
    toMarkdown (Ulist xls) = concatMap fromUL xls ++ "\n"
    toMarkdown (Olist xls) = concatMap fromOL xls ++ "\n"
    toMarkdown Nil         = ""

fromUL :: Xitem -> String
fromUL (Item str Nil) = "* " ++ fromXString str ++ "\n"
fromUL _              = error "Markdown does not support nested lists"

fromOL :: Xitem -> String
fromOL (Item str Nil) = "1. " ++ fromXString str ++ "\n"
fromOL _              = error "Markdown does not support nested lists"

fromXString :: [PString] -> String
fromXString = concatMap toStr
  where
    toStr Null          = ""
    toStr (R c)         = [c] -- xxx asterisk
    toStr (E c)         = [c] -- xxx
    toStr (L xs)        = xs -- xxx
    toStr (A title url) = "[" ++ title ++ "](" ++ url ++ ")"

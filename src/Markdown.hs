module Markdown (toMD) where

import Piki
import Types

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
    toMarkdown (DL ds)   = concatMap toMarkdown ds
    toMarkdown (IMG is)  = concatMap toMarkdown is
    toMarkdown (DIV _ els) = concatMap toMarkdown els

instance ToMD Def where
    toMarkdown (Def title desc) = "* " ++ fromXString title ++ "\n> " ++ fromXString desc ++ "\n"

instance ToMD Image where
    toMarkdown (Image title src) = "![" ++ title ++ "](" ++ src ++ ")\n"

instance ToMD Xlist where
    toMarkdown (Ulist xls) = concatMap toMarkdown xls
    toMarkdown (Olist xls) = concatMap toMarkdown xls
    toMarkdown Nil         = ""

instance ToMD Xitem where
    toMarkdown (Item str Nil) = "* " ++ fromXString str ++ "\n"
    toMarkdown _              = error "Markdown does not support nested lists"

fromXString :: [PString] -> String
fromXString = concatMap toStr
  where
    toStr Null          = ""
    toStr (R c)         = [c] -- xxx asterisk
    toStr (E c)         = [c] -- xxx
    toStr (L xs)        = xs -- xxx
    toStr (A title url) = "[" ++ title ++ "](" ++ url ++ ")"

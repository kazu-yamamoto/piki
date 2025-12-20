{-# LANGUAGE OverloadedStrings #-}

module Markdown (toMD) where

import Builder
import qualified Data.Text.Lazy as L
import Html ((\\\))
import Piki
import Types

----------------------------------------------------------------

toMD :: L.Text -> L.Text
toMD = build . mcatmap toMarkdown . piki

----------------------------------------------------------------

class ToMD a where
    toMarkdown :: a -> Builder

instance ToMD Element where
    toMarkdown HR = "********************************\n"
    toMarkdown (H n str) = toB (L.pack (replicate n '#')) +++ " " +++ fromXText str +++ "\n"
    toMarkdown (P str) = fromXText str +++ "\n\n"
    toMarkdown (PRE str) = fromXText str
    toMarkdown (UOL xl) = toMarkdown xl
    toMarkdown (DL ds) = mcatmap toMarkdown ds +++ "\n"
    toMarkdown (IMG is) = mcatmap toMarkdown is +++ "\n"
    toMarkdown (DIV (Class val) els) = "\n" +++ mcatmap toMarkdown els \\\ ("div", [("class", toB val)]) +++ "\n"
    toMarkdown (DIV (Id val) els) = "\n" +++ mcatmap toMarkdown els \\\ ("div", [("id", toB val)]) +++ "\n"
    toMarkdown (TABLE _) = "TABLE should be here\n"

instance ToMD Def where
    toMarkdown (Def title desc) = "* " +++ fromXText title +++ "\n> " +++ fromXText desc +++ "\n"

instance ToMD Image where
    toMarkdown (Image title src _) = "![" +++ toB title +++ "](" +++ toB src +++ ")\n"

instance ToMD Xlist where
    toMarkdown (Ulist xls) = mcatmap fromUL xls +++ "\n"
    toMarkdown (Olist xls) = mcatmap fromOL xls +++ "\n"
    toMarkdown Nil = ""

fromUL :: Xitem -> Builder
fromUL (Item str Nil) = "* " +++ fromXText str +++ "\n"
fromUL _ = error "Markdown does not support nested lists"

fromOL :: Xitem -> Builder
fromOL (Item str Nil) = "1. " +++ fromXText str +++ "\n"
fromOL _ = error "Markdown does not support nested lists"

fromXText :: XText -> Builder
fromXText = mcatmap toBld
  where
    toBld Null = ""
    toBld (R c) = toB (L.singleton c) -- xxx asterisk
    toBld (E c) = toB (L.singleton c) -- xxx
    toBld (L xs) = mcatmap toLine xs -- xxx
    toBld (A title url) = "[" +++ toB title +++ "](" +++ toB url +++ ")"
    toLine txt = "    " +++ toB txt +++ "\n"

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
    toMarkdown (H n str) = "\n" +++ toB (L.pack (replicate n '#')) +++ " " +++ fromXText str +++ "\n\n"
    toMarkdown (P str) = fromXText str +++ "\n\n"
    toMarkdown (PRE str) = "\n```\n" +++ fromXText' str +++ "```\n\n"
    toMarkdown (UOL xl) = toMarkdown xl
    toMarkdown (DL ds) = mcatmap toMarkdown ds +++ "\n"
    toMarkdown (IMG is) = mcatmap toMarkdown is +++ "\n"
    toMarkdown (DIV (Class val) els) = "\n::::{." +++ toB val +++ "}\n" +++ mcatmap toMarkdown els +++ "\n::::\n\n"
    toMarkdown (DIV (Id val) els) = "\n::::{#" +++ toB val +++ "}\n" +++ mcatmap toMarkdown els +++ "\n::::\n\n"
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
    toBld (R c) = referenceChar c
    toBld (E c) = toB (L.singleton c) -- xxx
    toBld (L xs) = mcatmap (\x -> reference x +++ "\n") xs
    toBld (A title url) = "[" +++ toB title +++ "](" +++ toB url +++ ")"

reference :: L.Text -> Builder
reference = mcatmap referenceChar . L.unpack

referenceChar :: Char -> Builder
referenceChar '<' = "\\<"
referenceChar '$' = "\\$"
referenceChar '#' = "\\#"
referenceChar '*' = "\\*"
referenceChar '\\' = "\\\\"
referenceChar '&' = "\\&"
referenceChar c = toB (L.singleton c)

fromXText' :: XText -> Builder
fromXText' = mcatmap toBld
  where
    toBld Null = ""
    toBld (R c) = toB (L.singleton c)
    toBld (E c) = toB (L.singleton c)
    toBld (L xs) = mcatmap toLine xs
    toBld (A title url) = "[" +++ toB title +++ "](" +++ toB url +++ ")"
    toLine txt = toB txt +++ "\n"

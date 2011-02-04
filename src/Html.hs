{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Html (toHTML, (\\\)) where

import Builder
import Data.Char
import Data.Maybe
import qualified Data.Text.Lazy as L
import Piki
import Types

----------------------------------------------------------------

toHTML :: L.Text -> L.Text -> L.Text
toHTML tmp inp = build $ replace tmp [("body",body),("title",fromXText title)]
  where
    elems = piki inp
    body = mcatmap toHtml elems
    title = getTitle elems

replace :: L.Text -> [(L.Text, Builder)] -> Builder
replace tmpl params = case L.break (== '$') tmpl of
  (s, "") -> toB s
  (s, cs) -> toB s +++ replaced +++ replace rest params
    where
      cs' = L.tail cs
      (var, rest') = L.span isAlpha cs'
      replaced = fromMaybe ("$" +++ toB var) (lookup var params)
      rest = chop rest'
      chop (L.stripPrefix "\n" -> Just xs) = xs
      chop xs                              = xs

----------------------------------------------------------------

class ToHtml a where
    toHtml :: a -> Builder

instance ToHtml Element where
    toHtml HR        = solo ("hr",[])
    toHtml (H n str) = fromXText str // ("h" +++ toB (L.pack (show lvl))) where lvl = min 6 n
    toHtml (P str)   = fromXText str // "p"
    toHtml (PRE str) = fromXText str \\ "pre"
    toHtml (UOL xl)  = toHtml xl
    toHtml (DL ds)   = mcatmap toHtml ds \\ "dl"
    toHtml (IMG is)  = mcatmap toHtml is
    toHtml (TABLE xss) = table
      where
        tds = map (mcatmap toTD) xss
        toTD x = fromXText x // "td"
        tr  = mconcat (map (\\ "tr") tds)
        table = tr \\ "table"
    toHtml (DIV (Class val) els) = mcatmap toHtml els \\\ ("div",[("class",toB val)])
    toHtml (DIV (Id val) els)    = mcatmap toHtml els \\\ ("div",[("id",toB val)])

instance ToHtml Def where
    toHtml (Def title desc) = fromXText title // "dt" +++ fromXText desc // "dd"

instance ToHtml Image where
    toHtml (Image title src murl) = case murl of
        Just url -> img %%% ("a",[("href", toB url)])
        Nothing   -> img
      where
        img = solo ("img",[("src",toB src),("alt",toB title),("title",toB title)])

instance ToHtml Xlist where
    toHtml xl = fromXlist 0 xl

fromXlist :: Int -> Xlist -> Builder
fromXlist c (Ulist xls) = mcatmap (fromXitem c) xls >\\ ("ul",c)
fromXlist c (Olist xls) = mcatmap (fromXitem c) xls >\\ ("ol",c)
fromXlist _ Nil         = ""

fromXitem :: Int -> Xitem -> Builder
fromXitem c (Item str Nil) = fromXText str >// ("li",c)
fromXitem c (Item str xls) = (fromXText str +++ fromXlist (c+1) xls) >// ("li",c)

----------------------------------------------------------------

fromXText :: XText -> Builder
fromXText = mcatmap toBld
  where
    toBld Null          = ""
    toBld (R c)         = referenceChar c
    toBld (E c)         = toB (L.singleton c)
    toBld (L xs)        = mcatmap (\x -> reference x +++ "\n") xs
    toBld (A title url) = reference title %%% ("a",[("href", toB url)])

reference :: L.Text -> Builder
reference = mcatmap referenceChar . L.unpack

referenceChar :: Char -> Builder
referenceChar '<' = "&lt;"
referenceChar '>' = "&gt;"
referenceChar '&' = "&amp;"
referenceChar '"' = "&quot;"
referenceChar c   = toB (L.singleton c)

----------------------------------------------------------------

infix 6 %%%, //, \\, \\\, >//, >\\ -- higher than +++

type TAGATTR = (Builder,[(Builder,Builder)])

open :: Builder -> Builder
open tag = "<" +++ tag +++ ">"

close :: Builder -> Builder
close tag = "</" +++ tag +++ ">"

(//) :: Builder -> Builder -> Builder
bld // tag = open tag +++ bld +++ close tag +++ "\n"

(\\) :: Builder -> Builder -> Builder
bld \\ tag = open tag +++ "\n" +++ bld +++ close tag +++ "\n"

(>//) :: Builder -> (Builder,Int) -> Builder
bld >// (tag,c) = bb +++ open tag +++ bld +++ close tag +++ "\n"
  where
    prefix = L.pack $ replicate c '\t'
    bb = toB prefix

(>\\) :: Builder -> (Builder,Int) -> Builder
bld >\\ (tag,c) = bb +++ open tag +++ "\n"
              +++ bld
              +++ toB prefix +++ close tag +++ "\n"
  where
    prefix = L.pack $ replicate c '\t'
    bb = if c == 0 then toB prefix else toB "\n" +++ toB prefix

(%%%) :: Builder -> TAGATTR -> Builder
bld %%% (tag,avs) = open (tag +++ attributes avs) +++ bld +++ close tag

(\\\) :: Builder -> TAGATTR -> Builder
bld \\\ (tag,avs) = open (tag +++ attributes avs) +++ "\n" +++ bld +++ close tag +++ "\n"

solo :: TAGATTR -> Builder
solo (tag,avs) = "<" +++ tag +++ attributes avs +++ " />\n"

attributes :: [(Builder,Builder)] -> Builder
attributes = mcatmap func
  where
    func (a,v) = " " +++ a +++ "=\"" +++ v +++ "\""


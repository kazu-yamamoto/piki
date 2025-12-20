{-# LANGUAGE OverloadedStrings #-}

module Piki (piki) where

import CharParser
import Data.Char
import Data.List (intercalate)
import qualified Data.Text.Lazy as L
import LineParser
import Notation
import Types

----------------------------------------------------------------

piki :: L.Text -> [Element]
piki line = either reportError id res
  where
    res = parse document "" $ L.lines line

----------------------------------------------------------------

document :: LineParser [Element]
document = clean *> many element <* eof

----------------------------------------------------------------

element :: LineParser Element
element =
    choice $
        map
            lexeme
            [ division
            , headline
            , hrule
            , uolist
            , dlist
            , image
            , table
            , preformatted
            , paragraph
            ]

----------------------------------------------------------------

headline :: LineParser Element
headline = do
    (lvl, txt) <- levelTitle <$> firstCharIs pikiTitle
    ttl <- getText txt
    return $ H lvl ttl
  where
    levelTitle line = (lvl, txt)
      where
        (mark, txt) = L.span (== pikiTitle) line
        lvl = fromIntegral $ L.length mark

----------------------------------------------------------------

hrule :: LineParser Element
hrule = HR <$ firstCharIs pikiHr

----------------------------------------------------------------

uolist :: LineParser Element
uolist = UOL <$> (ulist 1 <|> olist 1)

ulist :: Int -> LineParser Xlist
ulist n = Ulist <$> many1 (uitem n)

olist :: Int -> LineParser Xlist
olist n = Olist <$> many1 (oitem n)

uitem :: Int -> LineParser Xitem
uitem = xitem pikiUl

oitem :: Int -> LineParser Xitem
oitem = xitem pikiOl

xitem :: Char -> Int -> LineParser Xitem
xitem c n = Item <$> item c n <*> xlist (n + 1)

xlist :: Int -> LineParser Xlist
xlist n = ulist n <|> olist n <|> return Nil

item :: Char -> Int -> LineParser XText
item c n = (L.drop (fromIntegral n) <$> prefixIs pre) >>= getText
  where
    pre = L.pack $ replicate n c

----------------------------------------------------------------

dlist :: LineParser Element
dlist = DL <$> many1 ditem

ditem :: LineParser Def
ditem = Def <$> title <*> desc
  where
    title = firstCharIs' pikiDlT >>= getText
    desc = (firstCharIs' pikiDlD <?> ": no \"!\"") >>= getText

----------------------------------------------------------------

image :: LineParser Element
image = IMG <$> img
  where
    img = do
        rest <- firstCharIs' pikiImg
        case L.uncons rest of
            Just (x, rest')
                | x == pikiImg -> getTitleFileURLs rest'
                | otherwise -> getTitleFiles rest
            Nothing -> getTitleFiles rest

----------------------------------------------------------------

table :: LineParser Element
table = TABLE <$> tbl
  where
    tbl = many1 tr
    tr =
        firstCharIs' pikiTable
            >>= getTDs pikiTable pikiEscape
            >>= mapM getText

----------------------------------------------------------------

preformatted :: LineParser Element
preformatted = PRE . toXText <$> (open *> pre <* close)
  where
    open = prefixIs pikiPreOpen
    pre = many (prefixIsNot pikiPreClose)
    close = prefixIs pikiPreClose <?> ": no \"" ++ L.unpack pikiPreClose ++ "\""
    toXText xs = [L xs]

----------------------------------------------------------------

division :: LineParser Element
division = DIV <$> attr <*> elts <* close
  where
    attr = getAttr <$> firstCharIs' pikiDivOpen
    elts = many element
    close = firstCharIs pikiDivClose <?> ": no \"" ++ [pikiDivClose] ++ "\""
    getAttr value
        | L.any isUpper value = Id $ toLowerWord value
        | otherwise = Class value
    toLowerWord = L.map toLower

----------------------------------------------------------------

paragraph :: LineParser Element
paragraph = P . intercalate [R '\n'] <$> many1 paragLine

paragLine :: LineParser XText
paragLine = firstChar (`notElem` pikiReserved) >>= getText

----------------------------------------------------------------

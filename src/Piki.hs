module Piki (piki) where

import Data.Char
import Data.List (intersperse)
import Control.Monad
import Notation
import LineParser
import HtmlText
import Html

----------------------------------------------------------------

piki :: String -> (String,Maybe String)
piki line = either reportError id res
  where
    res = runParser document Nothing "" $ lines line

----------------------------------------------------------------

document :: LineParser (String,Maybe String)
document = (,) <$> html <*> title
  where
    html = clean *> (showElements <$> many element) <* eof
    showElements = concatMap show
    title = getState

----------------------------------------------------------------

element :: LineParser Element
element = choice $ map lexeme [
    division
  , headline
  , hrule
  , uolist
  , dlist
  , image
  , preformatted
  , paragraph
  ]

----------------------------------------------------------------

headline :: LineParser Element
headline = do
    (lvl,txt) <- levelTitle <$> firstCharIs pikiTitle
    ttl <- fromText txt
    setTitle lvl ttl
    return $ H lvl ttl
  where
    levelTitle line = (lvl, ttl)
      where
        (mark, ttl) = span (== pikiTitle) line
        lvl = length mark
    setTitle lvl ttl = when (lvl == 1) $ getState >>= setTitleIfNothing
      where
        setTitleIfNothing = maybe (setTtl ttl) doNothing
        setTtl = setState . Just
        doNothing _ = return ()

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

item :: Char -> Int -> LineParser String
item c n = drop n <$> prefixIs (replicate n c) >>= fromText

----------------------------------------------------------------

dlist :: LineParser Element
dlist = DL <$> many1 ditem

ditem :: LineParser Def
ditem = Def <$> title <*> desc
  where
    title = firstCharIs' pikiDlT >>= fromText
    desc  = (firstCharIs' pikiDlD <?> ": no \"!\"") >>= fromText

----------------------------------------------------------------

image :: LineParser Element
image = IMG <$> img
  where
    img = firstCharIs' pikiImg >>= getTitleFiles

----------------------------------------------------------------

preformatted :: LineParser Element
preformatted = toPre <$> (open *> pre <* close)
  where
    open  = prefixIs pikiPreOpen
    pre   = many (prefixIsNot pikiPreClose)
    close = prefixIs pikiPreClose <?> ": no \"" ++ pikiPreClose ++ "\""
    toPre = PRE . reference . unlines

----------------------------------------------------------------

division :: LineParser Element
division = DIV <$> attr <*> elts <* close
  where
    attr = getAttr <$> firstCharIs' pikiDivOpen
    elts = many element
    close = firstCharIs pikiDivClose <?> ": no \"" ++ [pikiDivClose] ++ "\""
    getAttr value
      | any isUpper value = Id $ toLowerWord value
      | otherwise         = Class value
    toLowerWord = map toLower

----------------------------------------------------------------

paragraph :: LineParser Element
paragraph = toP <$> many1 paragLine
  where
    toP = P . concat . intersperse "\n"

paragLine :: LineParser String
paragLine = firstChar (`notElem` pikiReserved) >>= fromText

----------------------------------------------------------------

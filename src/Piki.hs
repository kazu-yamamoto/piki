module Piki (piki) where

import Data.Char
import Data.List (intersperse)
import LineParser
import CharParser
import Notation
import Types

----------------------------------------------------------------

piki :: String -> [Element]
piki line = either reportError id res
  where
    res = parse document "" $ lines line

----------------------------------------------------------------

document :: LineParser [Element]
document = clean *> many element <* eof

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
    ttl <- getText txt
    return $ H lvl ttl
  where
    levelTitle line = (lvl,txt)
      where
        (mark, txt) = span (== pikiTitle) line
        lvl = length mark

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

item :: Char -> Int -> LineParser XString
item c n = (drop n <$> prefixIs (replicate n c)) >>= getText

----------------------------------------------------------------

dlist :: LineParser Element
dlist = DL <$> many1 ditem

ditem :: LineParser Def
ditem = Def <$> title <*> desc
  where
    title = firstCharIs' pikiDlT >>= getText
    desc  = (firstCharIs' pikiDlD <?> ": no \"!\"") >>= getText

----------------------------------------------------------------

image :: LineParser Element
image = IMG <$> img
  where
    img = firstCharIs' pikiImg >>= getTitleFiles

----------------------------------------------------------------

preformatted :: LineParser Element
preformatted = PRE . toXString <$> (open *> pre <* close)
  where
    open  = prefixIs pikiPreOpen
    pre   = unlines <$> many (prefixIsNot pikiPreClose)
    close = prefixIs pikiPreClose <?> ": no \"" ++ pikiPreClose ++ "\""
    toXString xs = [L xs]

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
paragraph = P <$> parag
  where
    parag = concat . intersperse [R '\n'] <$> many1 paragLine

paragLine :: LineParser XString
paragLine = firstChar (`notElem` pikiReserved) >>= getText

----------------------------------------------------------------

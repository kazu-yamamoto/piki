module Piki (piki) where

import Data.Char
import Data.List (intersperse)
import Control.Monad
import Notation
import LineParser
import HtmlText
import Html
import ApplicativeParsec

----------------------------------------------------------------

piki :: String -> (Maybe String,String)
piki line = case runParser document Nothing "" (lines line) of
              Right (title,html) -> (title,html)
              Left  err  -> reportError err

----------------------------------------------------------------

document :: LineParser (Maybe String,String)
document = do
    clean
    elts <- many element
    eof
    title <- getState
    let html = showElements elts
    return (title,html)
  where
    showElements = concatMap show

----------------------------------------------------------------

element :: LineParser Element
element = choice $ map lexeme [division, headline, hrule, uolist, dlist,
                               image, preformatted, paragraph]

----------------------------------------------------------------

headline :: LineParser Element
headline = do
    line <- firstCharIs pikiTitle
    let (lvl,txt) = levelTitle line
    ttl <- fromText txt
    setTitle lvl ttl
    return $ H lvl ttl
  where
    levelTitle line = let (mark, ttl) = span (== pikiTitle) line
                          lvl = length mark
                      in (lvl, ttl)
    setTitle lvl ttl = when (lvl == 1) $ do
                         title <- getState
                         case title of
                           Nothing -> setState (Just ttl)
                           Just _  -> return ()

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
item c n = do
    l <- prefixIs (replicate n c)
    let itm = drop n l
    fromText itm

----------------------------------------------------------------

dlist :: LineParser Element
dlist = DL <$> many1 ditem

ditem :: LineParser Def
ditem = do
    (_:ts) <- firstCharIs pikiDlT
    title  <- fromText ts
    (_:ds) <- firstCharIs pikiDlD <?> ": no \"!\""
    desc   <- fromText ds
    return $ Def title desc

----------------------------------------------------------------

image :: LineParser Element
image = do
    (_:line) <- firstCharIs pikiImg
    images <- getTitleFiles line
    return $ IMG images

----------------------------------------------------------------

preformatted :: LineParser Element
preformatted = do
    prefixIs pikiPreOpen
    ls <- many $ prefixIsNot pikiPreClose
    prefixIs pikiPreClose <?> ": no \"" ++ pikiPreClose ++ "\""
    let pre = toPre ls
    return $ PRE pre
  where
    toPre = reference . unlines

----------------------------------------------------------------

division :: LineParser Element
division = do
    (_:value) <- firstCharIs pikiDivOpen
    elts <- many element
    firstCharIs pikiDivClose <?> ": no \"" ++ [pikiDivClose] ++ "\""
    let attr = getAttr value
    return $ DIV attr elts
  where
    getAttr value = if anyUpperChar value
                    then (Id $ toLowerWord value)
                    else Class value
    anyUpperChar = or . map isUpper
    toLowerWord = map toLower

----------------------------------------------------------------

paragraph :: LineParser Element
paragraph = do
    ls <- many1 paragLine
    let p = toP ls
    return $ P p
  where
    toP = concat . intersperse "\n"

paragLine :: LineParser String
paragLine = firstChar (`notElem` pikiReserved) >>= fromText

----------------------------------------------------------------

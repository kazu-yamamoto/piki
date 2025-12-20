{-# LANGUAGE OverloadedStrings #-}

module LineParser (
    reportError,
    satisfy,
    firstChar,
    firstCharIs,
    firstCharIs',
    firstCharIsNot,
    prefixIs,
    prefixIsNot,
    lexeme,
    clean,
    module Parsec,
) where

import Data.Char
import qualified Data.Text.Lazy as L
import Notation
import Parsec
import Prelude hiding (all, null)

----------------------------------------------------------------

reportError :: ParseError -> a
reportError err =
    error $
        "error in line "
            ++ show (sourceLine (errorPos err) - 1)
            ++ concatMap msgString (errorMessages err)

msgString :: Message -> String
msgString (Expect str) = str
msgString (Message str) = str
msgString _ = ""

----------------------------------------------------------------

satisfy :: (L.Text -> Bool) -> LineParser L.Text
satisfy func = tokenPrim show next test
  where
    next pos _ _ = incSourceLine pos 1
    test txt
        | func txt = Just txt
        | otherwise = Nothing

firstChar :: (Char -> Bool) -> LineParser L.Text
firstChar func = satisfy $ test func
  where
    test f txt
        | L.null txt = False
        | otherwise = f (L.head txt)

firstCharIs :: Char -> LineParser L.Text
firstCharIs c = firstChar (== c)

firstCharIs' :: Char -> LineParser L.Text
firstCharIs' c = L.tail <$> firstCharIs c

firstCharIsNot :: Char -> LineParser L.Text
firstCharIsNot c = firstChar (/= c)

prefixIs :: L.Text -> LineParser L.Text
prefixIs pre = satisfy (pre `L.isPrefixOf`)

prefixIsNot :: L.Text -> LineParser L.Text
prefixIsNot pre = satisfy (not . L.isPrefixOf pre)

----------------------------------------------------------------

lexeme :: LineParser a -> LineParser a
lexeme p = p <* clean

clean :: LineParser ()
clean = skipMany (blank <|> comment)

blank :: LineParser L.Text
blank = satisfy $ L.all isSpace

comment :: LineParser L.Text
comment = firstCharIs pikiComment

module LineParser (Parser, LineParser, reportError, satisfy, 
                   firstChar, firstCharIs, firstCharIs', firstCharIsNot,
                   prefixIs, prefixIsNot,
                   lexeme, clean,
                   module Text.Parsec, module Control.Applicative) where

import Control.Applicative ((<$>),(<$),(<*),(<*>),(*>))
import Data.Char
import Data.List
import Notation
import Text.Parsec hiding (satisfy)
import Text.Parsec.Error
import Text.Parsec.String

----------------------------------------------------------------

type LineParser a = GenParser String (Maybe String) a

reportError :: ParseError -> a
reportError err = error $ "error in line "
               ++ show (sourceLine (errorPos err) - 1)
               ++ concatMap msgString (errorMessages err)

msgString :: Message -> String
msgString (Expect str)  = str
msgString (Message str) = str
msgString _             = ""

----------------------------------------------------------------

satisfy :: (String -> Bool) -> LineParser String
satisfy func = tokenPrim show next test
  where
    next pos _ _ = incSourceLine pos 1
    test s
     | func s    = Just s
     | otherwise = Nothing

firstChar :: (Char -> Bool) -> LineParser String
firstChar func = satisfy $ test func
    where
      test _ ""    = False
      test f (c:_) = f c

firstCharIs :: Char -> LineParser String
firstCharIs c = firstChar (== c)

firstCharIs' :: Char -> LineParser String
firstCharIs' c = tail <$> firstCharIs c

firstCharIsNot :: Char -> LineParser String
firstCharIsNot c = firstChar (/= c)

prefixIs :: String -> LineParser String
prefixIs str = satisfy (str `isPrefixOf`)

prefixIsNot :: String -> LineParser String
prefixIsNot str = satisfy (not . isPrefixOf str)

----------------------------------------------------------------

lexeme :: LineParser a -> LineParser a
lexeme p = p <* clean

clean :: LineParser ()
clean = skipMany (blank <|> comment)

blank :: LineParser String
blank = satisfy $ all isSpace

comment :: LineParser String
comment = firstCharIs pikiComment

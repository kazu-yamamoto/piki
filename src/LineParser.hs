module LineParser (LineParser, reportError, satisfy, 
                   firstChar, firstCharIs, firstCharIsNot,
                   prefixIs, prefixIsNot,
                   lexeme, clean,
                   module Text.ParserCombinators.Parsec) where

import Text.ParserCombinators.Parsec hiding (satisfy)
import Text.ParserCombinators.Parsec.Error
import Data.List
import Data.Char
import Notation

----------------------------------------------------------------

type LineParser a = GenParser String (Maybe String) a

reportError :: ParseError -> a
reportError err = error $ "error in line " ++
                  show (sourceLine (errorPos err) - 1) ++
                  concatMap msgString (errorMessages err)

msgString :: Message -> String
msgString (Expect str)  = str
msgString (Message str) = str
msgString _             = ""

----------------------------------------------------------------

satisfy :: (String -> Bool) -> LineParser String
satisfy func = tokenPrim
               (\s -> show s)
               (\pos _ _ -> incSourceLine pos 1)
               (\s -> if func s then Just s else Nothing)

firstChar :: (Char -> Bool) -> LineParser String
firstChar func = satisfy $ test func
    where
      test _ ""    = False
      test f (c:_) = f c

firstCharIs :: Char -> LineParser String
firstCharIs c = firstChar (== c)

firstCharIsNot :: Char -> LineParser String
firstCharIsNot c = firstChar (/= c)

prefixIs :: String -> LineParser String
prefixIs str = satisfy (str `isPrefixOf`)

prefixIsNot :: String -> LineParser String
prefixIsNot str = satisfy (not . isPrefixOf str)

----------------------------------------------------------------

lexeme :: LineParser a -> LineParser a
lexeme p = do x <- p
              clean
              return x

clean :: LineParser ()
clean = skipMany (blank <|> comment)

blank :: LineParser String
blank = satisfy $ all isSpace

comment :: LineParser String
comment = firstCharIs pikiComment
